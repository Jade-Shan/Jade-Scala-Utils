package jadeutils.comm.dao

import java.lang.RuntimeException

import java.sql.Connection
import java.sql.Savepoint

import jadeutils.common.Logging

import enumeratum.EnumEntry
import enumeratum.Enum
import java.sql.SQLException

/**
 * 事务隔离级别的抽象
 */
sealed abstract class TransIso(val id: Int, val name: String) extends EnumEntry
object TransIso extends Enum[TransIso] {
  val values = findValues // mandatory due to Enum extension
  val TransIso = findValues // mandatory due to Enum extension
  case object TS_NONE extends TransIso(Connection.TRANSACTION_NONE, "TRANSACTION_NONE")
  case object TS_READ_COMMITTED extends TransIso(Connection.TRANSACTION_READ_COMMITTED, "TRANSACTION_READ_COMMITTED")
  case object TS_READ_UNCOMMITTED extends TransIso(Connection.TRANSACTION_READ_UNCOMMITTED, "TRANSACTION_READ_UNCOMMITTED")
  case object TS_REPEATABLE_READ extends TransIso(Connection.TRANSACTION_REPEATABLE_READ, "TRANSACTION_REPEATABLE_READ")
  case object TS_SERIALIZABLE extends TransIso(Connection.TRANSACTION_SERIALIZABLE, "TRANSACTION_SERIALIZABLE")
}

/**
 * Transaction Nesting
 */
sealed abstract class TransNesting(val id: Int, val name: String) extends EnumEntry
object TransNesting extends Enum[TransNesting] {
  val values = findValues // mandatory due to Enum extension
  val TransNesting = findValues // mandatory due to Enum extension
  // PROPAGATION_REQUIRED -- 支持当前事务，如果当前没有事务，就新建一个事务。这是最常见的选择。
  case object TS_PG_REQUIRED extends TransNesting(0, "PROPAGATION_REQUIRED")
  // PROPAGATION_SUPPORTS -- 支持当前事务，如果当前没有事务，就以非事务方式执行。
  case object TS_PG_SUPPORTS extends TransNesting(1, "PROPAGATION_SUPPORTS")
  // PROPAGATION_MANDATORY -- 支持当前事务，如果当前没有事务，就抛出异常。
  case object TS_PG_MANDATORY extends TransNesting(2, "PROPAGATION_MANDATORY")
  // PROPAGATION_REQUIRES_NEW -- 新建事务，如果当前存在事务，把当前事务挂起。
  case object TS_PG_REQUIRES_NEW extends TransNesting(3, "PROPAGATION_REQUIRES_NEW")
  // PROPAGATION_NOT_SUPPORTED -- 以非事务方式执行操作，如果当前存在事务，就把当前事务挂起。
  case object TS_PG_NOT_SUPPORTED extends TransNesting(4, "PROPAGATION_NOT_SUPPORTED")
  // PROPAGATION_NEVER -- 以非事务方式执行，如果当前存在事务，则抛出异常。
  case object TS_PG_NEVER extends TransNesting(5, "PROPAGATION_NEVER")
  // PROPAGATION_NESTED -- 如果当前存在事务，则在嵌套事务内执行。如果当前没有事务，则进行与PROPAGATION_REQUIRED类似的操作。
  case object TS_PG_NESTED extends TransNesting(6, "PROPAGATION_NESTED")
}

case class TransactionEntry(val autoCommit: Boolean, val savepoint: Either[SQLException, Savepoint])

class DaoSession(
  val id: String, val conn: Connection, factory: DaoSessionFactory //
) extends Logging //
{

  private[this] var transStack: List[TransactionEntry] = Nil

  def lastTransaction(): Option[TransactionEntry] = {
    if (transStack.isEmpty) None else Some(transStack.head)
  }

  def pushTransaction(transEntry: TransactionEntry) {
    transStack = transEntry :: transStack
  }

  def popTransaction(): Option[TransactionEntry] = {
    val transEntry = lastTransaction()
    transStack = transStack.tail
    transEntry
  }

  def isBroken() = conn.isClosed

  def isInTrans() = transStack.isEmpty

  def close() { factory.closeSession(this) }

  override def toString = "(%s, %b)".format(id, isBroken)
}

abstract class DaoSessionFactory( //
  val minPoolSize: Int, val maxPoolSize: Int, val initPoolSize: Int //
) extends Logging //
{
  import jadeutils.comm.dao.TransIso.TransIso
  val defaultIsolation: TransIso

  private[this] var idleSess = List[DaoSession]()
  private[this] var actvSess = Map[String, DaoSession]()
  private[this] def size() = idleSess.size + actvSess.size
  private[this] var currSess = new ThreadLocal[DaoSession]

  def this() = this(20, 50, 20)

  // 创建JDBC连接
  def connectDB(): java.sql.Connection

  def currentSession = if (currSess.get != null &&
    !currSess.get.isBroken) //
    { // 返回JDBC连接没有断掉的会话
    currSess.get
  } else { // 关闭已经断掉的JDBC连接，再创建一个新的
    if (null != currSess.get)
      currSess.get.close
    createSession()
  }

  def createSession(): DaoSession = {
    if (size >= maxPoolSize)
      throw new RuntimeException("Db connection Pool filled")

    val sess = getAvaliable() // 取一个可用的连接
    actvSess = actvSess + (sess.id -> sess)
    currSess.set(sess)

    logTrace(
      "after create session: size: {} ----- max: {}\nidle: {}\nactive: {}",
      size, maxPoolSize, idleSess, actvSess)
    sess
  }

  private[this] def getAvaliable(): DaoSession = {
    val sess = if (idleSess.size < 1) {
      // 没有空闲的连接就新建一个
      new DaoSession("" + size, connectDB(), this)
    } else {
      // 有空闲的连接就取一个
      var first = idleSess.head
      idleSess = idleSess.tail
      first
    }
    // drop borken session, find next idle session
    if (sess.isBroken) getAvaliable() else sess
  }

  def closeSession(sess: DaoSession) {
    if (actvSess.contains(sess.id)) {
      actvSess = actvSess - sess.id
      idleSess = sess :: idleSess
    }
    logTrace(
      "after close session: size: {} ----- max: {}\nidle: {}\nactive: {}",
      size, maxPoolSize, idleSess, actvSess)
  }

}

abstract class BaseTransactionService extends Logging {
  import scala.reflect.runtime.universe.Type
  import scala.reflect.runtime.universe.typeOf
  import scala.reflect.runtime.universe.TypeTag
  import jadeutils.comm.dao.TransNesting._
  import jadeutils.comm.dao.TransIso._

  protected val sessionFactory: DaoSessionFactory

  @throws(classOf[SQLException])
  def withTransaction[T]( //
    autoCommit: Boolean      = false,          // 不自动提交
    nesting:    TransNesting = TS_PG_REQUIRED, // 默认加入外层事务
    iso:        TransIso     = TS_SERIALIZABLE // 默认事务隔离级别为顺序
  ) (callFunc: => T                            // 事务中的具体操作
  ) (implicit m: TypeTag[T]): T = {            // 隐式参数自动匹配被事务包裹函数的返回类型
    warpSession(autoCommit, nesting, iso, callFunc)
  }

  @throws(classOf[SQLException])
  def withTransaction[T](callFunc: => T)(implicit m: TypeTag[T]): T = {
    warpSession(false, TS_PG_REQUIRED, sessionFactory.defaultIsolation, callFunc)
  }

  @throws(classOf[SQLException])
  private def warpSession[T]( //
    autoCommit: Boolean,      //
    nesting:    TransNesting, //
    iso:        TransIso,     //
    callFunc:   => T          //
  ) (implicit m: TypeTag[T]): T = {
    val sess = sessionFactory.currentSession

    dealwithTransNesting(sess, nesting)
    val lastTrans = if (sess.lastTransaction().isEmpty) {
      TransactionEntry(true, null)
    } else sess.lastTransaction().get

    val isAutoCommit = sess.lastTransaction().get.autoCommit

    sess.conn.setTransactionIsolation(iso.id)
    sess.conn.setAutoCommit(isAutoCommit)
    logTrace("Trans begin: S: {}", sess.id)

    var result: Either[Throwable, T] = try {
      var funcResult = callFunc
      if (!isAutoCommit) {
        sess.conn.commit()
        logTrace("Trans commit: S: {}", sess.id)
      }
      Right(funcResult)
    } catch {
      case e: RuntimeException => {
        if (!isAutoCommit) {
          val lastTrans = sess.lastTransaction().getOrElse(null)
          if (null != lastTrans && lastTrans.savepoint.isRight) {
            sess.conn.rollback(lastTrans.savepoint.right.get)
          } else sess.conn.rollback()
          logTrace("Trans rollback: S: {}", sess.id)
        }
        Left(e)
      }
      case e: Throwable => {
        if (!isAutoCommit) {
          sess.conn.commit()
          logTrace("Trans commit: S: {}", sess.id)
        }
        Left(e)
      }
    } finally {
      sess.popTransaction()
      logTrace("Trans end: S: {}", sess.id)
    }
    
    if (result.isLeft) throw result.left.get else {
      result.right.get.asInstanceOf[T]
    }

  }

  @throws(classOf[SQLException])
  private[this] def dealwithTransNesting(sess: DaoSession, nesting: TransNesting): Unit = {
    def newTransaction(): Either[SQLException, Savepoint] = try {
      Right(sess.conn.setSavepoint("" + System.currentTimeMillis()))
    } catch {
      case e: SQLException => Left(e)
    }
    nesting match {
      case TS_PG_NEVER => if (!sess.lastTransaction().isEmpty) {
        // PROPAGATION_NEVER -- 以非事务方式执行，如果当前存在事务，则抛出异常。
        throw new SQLException("Expect not in any transaction");
      } else (null, null)
      case TS_PG_MANDATORY => if (sess.lastTransaction().isEmpty) {
        // PROPAGATION_MANDATORY -- 支持当前事务，如果当前没有事务，就抛出异常。
        throw new SQLException("Expect in transaction");
      }
      case TS_PG_REQUIRED => if (sess.lastTransaction().isEmpty) {
        // PROPAGATION_REQUIRED -- 支持当前事务，如果当前没有事务，就新建一个事务。这是最常见的选择。
        sess.pushTransaction(TransactionEntry(false, newTransaction))
      }
      case TS_PG_SUPPORTS => if (!sess.lastTransaction().isEmpty) {
        // PROPAGATION_SUPPORTS -- 支持当前事务，如果当前没有事务，就以非事务方式执行。
        sess.pushTransaction(TransactionEntry(true, null))
      }
      case TS_PG_REQUIRES_NEW => {
        // PROPAGATION_REQUIRES_NEW -- 新建事务，如果当前存在事务，把当前事务挂起。
        sess.pushTransaction(TransactionEntry(false, newTransaction))
      }
      case TS_PG_NOT_SUPPORTED => {
        // PROPAGATION_NOT_SUPPORTED -- 以非事务方式执行操作，如果当前存在事务，就把当前事务挂起。
        val newPoint = sess.conn.setSavepoint("" + System.currentTimeMillis())
        sess.pushTransaction(TransactionEntry(true, null))
      }
      case TS_PG_NESTED => {
        // PROPAGATION_NESTED -- 如果当前存在事务，则在嵌套事务内执行。
        // 如果当前没有事务，则进行与PROPAGATION_REQUIRED类似的操作。
        sess.pushTransaction(TransactionEntry(false, newTransaction))
      }
      case _ => throw new SQLException("Unknow Trans Prop")
    }
  }

  private[this] def generateDefaultResult(m: Type): Any = m match {
    case t if (t <:< typeOf[Byte])    => 0
    case t if (t <:< typeOf[Short])   => 0
    case t if (t <:< typeOf[Int])     => 0
    case t if (t <:< typeOf[Long])    => 0L
    case t if (t <:< typeOf[Float])   => 0F
    case t if (t <:< typeOf[Double])  => 0
    case t if (t <:< typeOf[Char])    => '0'
    case t if (t <:< typeOf[Boolean]) => false
    case t if (t <:< typeOf[Unit])    => ()
    case _                            => null
  }

}


