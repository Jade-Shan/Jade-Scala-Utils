package jadeutils.comm.dao

import java.lang.RuntimeException

import java.sql.Connection
import java.sql.Savepoint

import jadeutils.common.Logging

import enumeratum.EnumEntry
import enumeratum.Enum

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

abstract class TransactionLayer(
	val autoCommit: Boolean,                        //
	val savepoint: Either[Throwable, Savepoint]  //
)

class NewTransactionLayer(savepoint: Either[Throwable, Savepoint])
	extends TransactionLayer(false, savepoint)

class JoinLastTransaction extends TransactionLayer(false, null)

class NoTransactionLayer(savepoint: Either[Throwable, Savepoint])
	extends TransactionLayer(true, savepoint) //
{
	def this() = this(null)
}

class DaoSession(val id: String, val conn: Connection, pool: DaoSessionPool //
) extends Logging //
{
	private[this] var transStack: List[TransactionLayer] = Nil

	private[this] def currTransactionLayer(): Option[TransactionLayer] = {
		if (transStack.isEmpty) None else Some(transStack.head)
	}

	def pushTransaction(transEntry: TransactionLayer) {
		transStack = transEntry :: transStack
	}

	def popTransaction(): Option[TransactionLayer] = {
		val transEntry = currTransactionLayer
		transStack = transStack.tail
		if (!isInTransaction) this.close() // 全部事务完成，关闭会话
		transEntry
	}

	def isBroken() = conn.isClosed

	def isInTransaction() = if (transStack.isEmpty) false else transStack.head match {
		case l: NewTransactionLayer => true
		case l: JoinLastTransaction => true
		case l: NoTransactionLayer  => false
	}

	def close() { pool.returnBack(this) }

	override def toString = "(%s, %b)".format(id, isBroken)
}

abstract class DaoSessionPool(val minPoolSize: Int, val maxPoolSize: Int, val initPoolSize: Int //
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
	protected[this] def connectDB(): Either[RuntimeException, java.sql.Connection]

	def current: Either[RuntimeException, DaoSession] = {
		var s = currSess.get
		if (null == s) borrow() else if (!s.isBroken()) Right(s) else {
			currSess.get.close
			borrow()
		}
	}

	/**
	 * 从连接池中拿一个可用的会话
	 */
	def borrow(): Either[RuntimeException, DaoSession] = {
		def getAvaliable(): Either[RuntimeException, DaoSession] = {
			val sess = if (idleSess.size < 1) {
				// 没有空闲的连接就新建一个
				val dbConn = connectDB()
				if (dbConn.isLeft) Left(dbConn.left.get)
				else Right(new DaoSession("" + size, dbConn.right.get, this))
			} else {
				// 有空闲的连接就取一个
				var first = idleSess.head
				idleSess = idleSess.tail
				Right(first)
			}
			// drop borken session, find next idle session
			if (sess.isRight && !sess.right.get.isBroken()) sess else getAvaliable()
		}
		if (size >= maxPoolSize) {
			Left(new RuntimeException("Db connection Pool filled"))
		} else {
			val pse = getAvaliable() // 取一个可用的连接
			if (pse.isLeft) pse else {
				val sess = pse.right.get
				actvSess = actvSess + (sess.id -> sess)
				currSess.set(sess)

				logTrace("after create session: size: {} ----- max: {}\nidle: {}\nactive: {}",
				size, maxPoolSize, idleSess, actvSess
				)
				Right(sess)
			}
		}
	}

	/**
	 * 查询完成，放回连接池
	 */
	def returnBack(sess: DaoSession) {
		if (actvSess.contains(sess.id) && !actvSess.get(sess.id).get.isInTransaction()) {
			actvSess = actvSess - sess.id
			idleSess = sess :: idleSess
			currSess.remove()
		}
		logTrace("after close session: size: {} ----- max: {}\nidle: {}\nactive: {}",
		size, maxPoolSize, idleSess, actvSess
		)
	}

}

abstract class BaseTransactionService extends Logging {
	import scala.reflect.runtime.universe.Type
	import scala.reflect.runtime.universe.typeOf
	import scala.reflect.runtime.universe.TypeTag
	import jadeutils.comm.dao.TransNesting._
	import jadeutils.comm.dao.TransIso._

	protected val daoSessPool: DaoSessionPool

	def withTransaction[T]( //
			nesting: TransNesting = TS_PG_REQUIRED,     // 默认加入外层事务
			iso: TransIso = TS_SERIALIZABLE             // 默认事务隔离级别为顺序
	)(callFunc: => Either[Throwable, T] // 事务中的具体操作
	)(implicit m: TypeTag[T]): Either[Throwable, T] = { // 隐式参数自动匹配被事务包裹函数的返回类型
		warpSession(nesting, iso, callFunc)
	}

	def withTransaction[T](callFunc: => Either[Throwable, T])(implicit m: TypeTag[T]): Either[Throwable, T] = {
		warpSession(TS_PG_REQUIRED, daoSessPool.defaultIsolation, callFunc)
	}

	private def warpSession[T](nesting: TransNesting, iso: TransIso, 
			callFunc: => Either[Throwable, T])
		(implicit m: TypeTag[T]): Either[Throwable, T] = //
	{
		val sessT = daoSessPool.current
		if (sessT.isLeft) {
			logError("Lost Connection from database")
			Left(new RuntimeException("Lost Connection from database"))
		}

		val sess = sessT.right.get
		dealwithTransNesting(sess, nesting)
		//		val lastTrans = if (sess.lastTransaction().isEmpty) {
		//		TransactionEntry(true, Left(new RuntimeException("not in transaction")))
		//		} else sess.lastTransaction().get
		//
		val isAutoCommit = !sess.isInTransaction

		sess.conn.setTransactionIsolation(iso.id)
		sess.conn.setAutoCommit(isAutoCommit)
		logTrace("Trans begin: S: {}", sess.id)

		val result: Either[Throwable, T] = try callFunc catch { case e: Throwable => Left(e) }

		val currTransLayer = sess.popTransaction();

		if (result.isRight) currTransLayer match {
			case Some(l: NewTransactionLayer) => {
				logTrace("Call Func Success, start commit transaction manually: S: {}", sess.id)
				sess.conn.commit()
				logTrace("Call Func Success, commit transaction manually success: S: {}", sess.id)
				result
			}
			case Some(l: JoinLastTransaction) => {
				logTrace("Call Func Success, retrun outter transaction : S: {}", sess.id)
				result
			}
			case Some(_) => {
				logTrace("Call Func Success, not in transaction: S: {}", sess.id)
				result
			}
			case None => {
				logTrace("Call Func Success, not in transaction: S: {}", sess.id)
				result
			}
		} else currTransLayer match {
			case Some(l: NewTransactionLayer) => {
				logTrace("Call Func Err, Trans rollback: S: {} for err: {}", result.left.get)
				if (null != l && l.savepoint.isRight) {
					sess.conn.rollback(l.savepoint.right.get)
				} else sess.conn.rollback()
				result
			}
			case Some(l: JoinLastTransaction) => {
				logTrace("Call Func Err, need rollback outter transaction : S: {}", sess.id)
				// TODO: 如何回滚到外面一层事务？？？
			}
			case Some(_) => {
				logTrace("Call Func Err, need rollback outter transaction : S: {}", sess.id)
				// TODO: 如何回滚到外面一层事务？？？
			}
			case None => {
				logTrace("Call Func Err, not in transaction so no rollback: S: {}", sess.id)
			}
		}

		result
	}

	private[this] def dealwithTransNesting(sess: DaoSession, nesting: TransNesting): Unit = {
		def createSavepoint(): Either[Throwable, Savepoint] = try {
			Right(sess.conn.setSavepoint("" + System.currentTimeMillis()))
		} catch {
			case e: Throwable => Left(e)
		}
		nesting match {
			case TS_PG_NEVER => if (sess.isInTransaction) {
				// PROPAGATION_NEVER -- 以非事务方式执行，如果当前存在事务，则抛出异常。
				throw new RuntimeException("Expect not in any transaction");
			} else sess.pushTransaction(new NoTransactionLayer)
			case TS_PG_MANDATORY => if (sess.isInTransaction) {
				// PROPAGATION_MANDATORY -- 支持当前事务，如果当前没有事务，就抛出异常。
				sess.pushTransaction(new JoinLastTransaction)
			} else throw new RuntimeException("Expect in transaction but not");
			case TS_PG_REQUIRED => if (sess.isInTransaction) {
				// PROPAGATION_REQUIRED -- 支持当前事务，如果当前没有事务，就新建一个事务。这是最常见的选择。
				sess.pushTransaction(new JoinLastTransaction)
			} else sess.pushTransaction(new NewTransactionLayer(createSavepoint))
			case TS_PG_NESTED => if (!sess.isInTransaction) {
				// PROPAGATION_NESTED -- 如果当前存在事务，则在嵌套事务内执行。
				// 如果当前没有事务，则进行与PROPAGATION_REQUIRED类似的操作。
				sess.pushTransaction(new NewTransactionLayer(createSavepoint))
			}
			case TS_PG_SUPPORTS => if (sess.isInTransaction) {
				// PROPAGATION_SUPPORTS -- 支持当前事务，如果当前没有事务，就以非事务方式执行。
				sess.pushTransaction(new JoinLastTransaction)
			} else sess.pushTransaction(new NoTransactionLayer)
			case TS_PG_REQUIRES_NEW => {
				// PROPAGATION_REQUIRES_NEW -- 新建事务，如果当前存在事务，把当前事务挂起。
				sess.pushTransaction(new NewTransactionLayer(createSavepoint))
			}
			case TS_PG_NOT_SUPPORTED => {
				// PROPAGATION_NOT_SUPPORTED -- 以非事务方式执行操作，如果当前存在事务，就把当前事务挂起。
				sess.pushTransaction(new NoTransactionLayer)
			}
			case _ => throw new RuntimeException("UnSupport Transaction Type")
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
		case _ => null
	}

}


