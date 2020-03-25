package jadeutils.comm.dao

import java.lang.RuntimeException
import java.sql.Connection

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
	case object TS_NONE             extends TransIso(Connection.TRANSACTION_NONE, "TRANSACTION_NONE")
	case object TS_READ_COMMITTED   extends TransIso(Connection.TRANSACTION_READ_COMMITTED, "TRANSACTION_READ_COMMITTED")
	case object TS_READ_UNCOMMITTED extends TransIso(Connection.TRANSACTION_READ_UNCOMMITTED, "TRANSACTION_READ_UNCOMMITTED")
	case object TS_REPEATABLE_READ  extends TransIso(Connection.TRANSACTION_REPEATABLE_READ, "TRANSACTION_REPEATABLE_READ")
	case object TS_SERIALIZABLE     extends TransIso(Connection.TRANSACTION_SERIALIZABLE, "TRANSACTION_SERIALIZABLE")           
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



class DaoSession(val id: String, val conn: Connection, 
	factory: DaoSessionFactory) extends Logging 
{
	import java.sql.Savepoint
	
	private[this] var savepoints: List[Savepoint] = Nil

	def lastSavepoint(): Option[Savepoint] = if (savepoints.isEmpty) None else {
		Some(savepoints.head)
	}
	
	def pushSavepoint(savepoint: Savepoint) {
		savepoints = savepoint :: savepoints
	}
	
	def popSavepoint(savepoint: Savepoint) {
		if(!savepoints.isEmpty && (savepoints.head eq savepoint)) {
			savepoints = savepoints.tail
		}
	}

	def isBroken() = conn.isClosed

	def isInTrans() = savepoints.isEmpty

	def close() { factory.closeSession(this) }

	override def toString = "(%s, %b)".format(id, isBroken)
}

abstract class DaoSessionFactory(val minPoolSize: Int, val maxPoolSize: Int, 
	val initPoolSize: Int) extends Logging 
{ 
	import jadeutils.comm.dao.TransIso.TransIso
	val defaultIsolation: TransIso

	private[this] var idleSess = List[DaoSession]()
	private[this] var actvSess = Map[String, DaoSession]()
	private[this] def size()   = idleSess.size + actvSess.size
	private[this] var currSess = new ThreadLocal[DaoSession]

	def this() = this(20, 50, 20)

	// 创建JDBC连接
	def connectDB() : java.sql.Connection

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
		val sess  = if (idleSess.size < 1) {
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
	import java.sql.SQLException
	import java.sql.Savepoint
	import scala.reflect.runtime.universe.Type
	import scala.reflect.runtime.universe.typeOf
	import scala.reflect.runtime.universe.TypeTag
	import jadeutils.comm.dao.TransNesting._
	import jadeutils.comm.dao.TransIso._

	protected val sessionFactory: DaoSessionFactory

	@throws(classOf[SQLException])
	def withTransaction[T](autoCommit: Boolean = false, 
		nesting: TransNesting = TS_PG_REQUIRED, 
		iso: TransIso = TS_SERIALIZABLE)(callFunc: => T)
	(implicit m: TypeTag[T]): T =  //
	{ warpSession(autoCommit, nesting, iso, callFunc) }

	@throws(classOf[SQLException])
	def withTransaction[T](callFunc: => T)(implicit m: TypeTag[T]): T = {
		warpSession(false, TS_PG_REQUIRED, sessionFactory.defaultIsolation, 
			callFunc)
	}

	@throws(classOf[SQLException])
	private def warpSession[T](autoCommit: Boolean, nesting: TransNesting, 
		iso: TransIso, callFunc: => T)(implicit m: TypeTag[T]): T =  //
	{
		val sess = sessionFactory.currentSession
		val autoCommitBackup = sess.conn.getAutoCommit

		val savepoint = dealwithTransNesting(sess, nesting)
//		if (!sess.isInTrans) {
		if (null != savepoint._1) {
//			sess.isInTrans = true
			sess.conn.setTransactionIsolation(iso.id)
			sess.conn.setAutoCommit(false)
			logTrace("Trans begin: S: {}", sess.id)
		}

		var result = try {
			var funcResult = callFunc
//			if (sess.isInTrans) {
			if (null != savepoint._1) {
				sess.conn.commit()
				logTrace("Trans commit: S: {}", sess.id)
			}
			(funcResult, null)
		} catch {
			case e: RuntimeException => {
				if (sess.isInTrans) {
					if(null != savepoint._1) {
						sess.conn.rollback(savepoint._1)
					} else if (null != savepoint._2) {
						sess.conn.rollback(savepoint._2)
					} else {
						sess.conn.rollback()
					}
					logTrace("Trans rollback: S: {}", sess.id)
				}
				(generateDefaultResult(typeOf[T]), e)
			}
		} finally {
			sess.conn.setAutoCommit(autoCommitBackup)
			//			sess.isInTrans = false
			if (null != savepoint._1) { sess.pushSavepoint(savepoint._1) }
			if (!sess.isInTrans) { sess.close }
			logTrace("Trans end: S: {}", sess.id)
		}

		if (null != result._2) throw result._2

		result._1.asInstanceOf[T]
	}


	@throws(classOf[SQLException])
	private[this] def dealwithTransNesting(sess: DaoSession, nesting: TransNesting): (Savepoint, Savepoint) = {
		val lastPoint = sess.lastSavepoint().getOrElse(null)
		nesting match {
			// PROPAGATION_NEVER -- 以非事务方式执行，如果当前存在事务，则抛出异常。
			case TS_PG_NEVER => if (null != lastPoint) {
				sess.conn.setAutoCommit(true)
				throw new SQLException("Trans NEVER but in session");
			} else (null, null)
			// PROPAGATION_MANDATORY -- 支持当前事务，如果当前没有事务，就抛出异常。
			case TS_PG_MANDATORY => if (null == lastPoint) {
				throw new SQLException("Trans MANDATORY but not in session");
			} else (null, lastPoint)
			// PROPAGATION_REQUIRED -- 支持当前事务，如果当前没有事务，就新建一个事务。这是最常见的选择。
			case TS_PG_REQUIRED => if (null != lastPoint) (null, lastPoint) else {
				val newPoint = sess.conn.setSavepoint("" + System.currentTimeMillis())
				sess.pushSavepoint(newPoint)
				(newPoint, null)
			}
			// PROPAGATION_SUPPORTS -- 支持当前事务，如果当前没有事务，就以非事务方式执行。
			case TS_PG_SUPPORTS => if (null != lastPoint) (null, lastPoint) else {
				sess.conn.setAutoCommit(true)
				(null, null)
			}
			// PROPAGATION_REQUIRES_NEW -- 新建事务，如果当前存在事务，把当前事务挂起。
			case TS_PG_REQUIRES_NEW => {
				val newPoint = sess.conn.setSavepoint("" + System.currentTimeMillis())
//				sess.pushSavepoint(newPoint)
				(newPoint, null)
			}
			// PROPAGATION_NOT_SUPPORTED -- 以非事务方式执行操作，如果当前存在事务，就把当前事务挂起。
			case TS_PG_NOT_SUPPORTED => {
				sess.conn.setAutoCommit(true)
				val newPoint = sess.conn.setSavepoint("" + System.currentTimeMillis())
//				sess.pushSavepoint(newPoint)
				(newPoint, null)
			}
			// PROPAGATION_NESTED -- 如果当前存在事务，则在嵌套事务内执行。如果当前没有事务，则进行与PROPAGATION_REQUIRED类似的操作。
			case TS_PG_NESTED => if (null == lastPoint) {
					val newPoint = sess.conn.setSavepoint("" + System.currentTimeMillis())
					sess.pushSavepoint(newPoint)
					(newPoint, null)
				} else (null, lastPoint)
			case _ => throw new SQLException("Unknow Trans Prop")
		}
	}

	private[this] def generateDefaultResult(m: Type): Any = m match {
			case t if (t <:< typeOf[Byte   ]) => 0
			case t if (t <:< typeOf[Short  ]) => 0
			case t if (t <:< typeOf[Int    ]) => 0
			case t if (t <:< typeOf[Long   ]) => 0L
			case t if (t <:< typeOf[Float  ]) => 0F
			case t if (t <:< typeOf[Double ]) => 0
			case t if (t <:< typeOf[Char   ]) => '0'
			case t if (t <:< typeOf[Boolean]) => false
			case t if (t <:< typeOf[Unit   ]) => { () }
			case _                            => null
	}

}


