package jadeutils.comm.dao

import java.lang.RuntimeException
import java.sql.Connection

import jadeutils.common.Logging

import enumeratum.EnumEntry
import enumeratum.Enum

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


// PROPAGATION_REQUIRED -- 支持当前事务，如果当前没有事务，就新建一个事务。这是最常见的选择。
// PROPAGATION_SUPPORTS -- 支持当前事务，如果当前没有事务，就以非事务方式执行。
// PROPAGATION_MANDATORY -- 支持当前事务，如果当前没有事务，就抛出异常。
// PROPAGATION_REQUIRES_NEW -- 新建事务，如果当前存在事务，把当前事务挂起。
// PROPAGATION_NOT_SUPPORTED -- 以非事务方式执行操作，如果当前存在事务，就把当前事务挂起。
// PROPAGATION_NEVER -- 以非事务方式执行，如果当前存在事务，则抛出异常。
// PROPAGATION_NESTED -- 如果当前存在事务，则在嵌套事务内执行。如果当前没有事务，则进行与PROPAGATION_REQUIRED类似的操作。
/**
 * Transaction Nesting
 */
sealed abstract class TransNesting(val id: Int, val name: String) extends EnumEntry
object TransNesting extends Enum[TransNesting] {
  val values = findValues // mandatory due to Enum extension
  val TransNesting = findValues // mandatory due to Enum extension
	case object TS_PG_REQUIRED      extends TransNesting(0, "PROPAGATION_REQUIRED")
	case object TS_PG_SUPPORTS      extends TransNesting(1, "PROPAGATION_SUPPORTS")
	case object TS_PG_MANDATORY     extends TransNesting(2, "PROPAGATION_MANDATORY")
	case object TS_PG_REQUIRES_NEW  extends TransNesting(3, "PROPAGATION_REQUIRES_NEW")
	case object TS_PG_NOT_SUPPORTED extends TransNesting(4, "PROPAGATION_NOT_SUPPORTED")
	case object TS_PG_NEVER         extends TransNesting(5, "PROPAGATION_NEVER")
	case object TS_PG_NESTED        extends TransNesting(6, "PROPAGATION_NESTED")
}



class DaoSession(val id: String, val connection: Connection, 
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

	def isBroken() = connection.isClosed
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
	private[this] var currSession = new ThreadLocal[DaoSession]

	def this() = this(20, 50, 20)

	def createConnection() : java.sql.Connection

	def currentSession = if (currSession.get != null &&
		!currSession.get.isBroken) //
	{
		currSession.get
	} else {
		if (null != currSession.get)
			currSession.get.close
		createSession()
	}

	def createSession(): DaoSession = {
		if (size >= maxPoolSize) 
			throw new RuntimeException("Db connection Pool filled")

		val sess = nextSession()
		actvSess = actvSess + (sess.id -> sess)
		currSession.set(sess)

		logTrace(
			"after create session: size: {} ----- max: {}\nidle: {}\nactive: {}", 
			size, maxPoolSize, idleSess, actvSess)
		sess
	}

	private[this] def nextSession(): DaoSession = {
		val sess  = if (idleSess.size < 1) {
			new DaoSession("" + size, createConnection(), this)
		} else {
			var first = idleSess.head
			idleSess = idleSess.tail
			first
		}
		if (sess.isBroken) {
			nextSession()  // drop borken session, find next idle session
		} else sess
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
		val autoCommitBackup = sess.connection.getAutoCommit

		val savepoint = dealwithTransNesting(sess, nesting)
//		if (!sess.isInTrans) {
		if (null != savepoint._1) {
//			sess.isInTrans = true
			sess.connection.setTransactionIsolation(iso.id)
			sess.connection.setAutoCommit(false)
			logTrace("Trans begin: S: {}", sess.id)
		}

		var result = try {
			var funcResult = callFunc
//			if (sess.isInTrans) {
			if (null != savepoint._1) {
				sess.connection.commit()
				logTrace("Trans commit: S: {}", sess.id)
			}
			(funcResult, null)
		} catch {
			case e: RuntimeException => {
				if (sess.isInTrans) {
					if(null != savepoint._1) {
						sess.connection.rollback(savepoint._1)
					} else if (null != savepoint._2) {
						sess.connection.rollback(savepoint._2)
					} else {
						sess.connection.rollback()
					}
					logTrace("Trans rollback: S: {}", sess.id)
				}
				(generateDefaultResult(typeOf[T]), e)
			}
		} finally {
			sess.connection.setAutoCommit(autoCommitBackup)
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
				sess.connection.setAutoCommit(true)
				throw new SQLException("Trans NEVER but in session");
			} else (null, null)
			// PROPAGATION_MANDATORY -- 支持当前事务，如果当前没有事务，就抛出异常。
			case TS_PG_MANDATORY => if (null == lastPoint) {
				throw new SQLException("Trans MANDATORY but not in session");
			} else (null, lastPoint)
			// PROPAGATION_REQUIRED -- 支持当前事务，如果当前没有事务，就新建一个事务。这是最常见的选择。
			case TS_PG_REQUIRED => if (null != lastPoint) (null, lastPoint) else {
				val newPoint = sess.connection.setSavepoint("" + System.currentTimeMillis())
				sess.pushSavepoint(newPoint)
				(newPoint, null)
			}
			// PROPAGATION_SUPPORTS -- 支持当前事务，如果当前没有事务，就以非事务方式执行。
			case TS_PG_SUPPORTS => if (null != lastPoint) (null, lastPoint) else {
				sess.connection.setAutoCommit(true)
				(null, null)
			}
			// PROPAGATION_REQUIRES_NEW -- 新建事务，如果当前存在事务，把当前事务挂起。
			case TS_PG_REQUIRES_NEW => {
				val newPoint = sess.connection.setSavepoint("" + System.currentTimeMillis())
//				sess.pushSavepoint(newPoint)
				(newPoint, null)
			}
			// PROPAGATION_NOT_SUPPORTED -- 以非事务方式执行操作，如果当前存在事务，就把当前事务挂起。
			case TS_PG_NOT_SUPPORTED => {
				sess.connection.setAutoCommit(true)
				val newPoint = sess.connection.setSavepoint("" + System.currentTimeMillis())
//				sess.pushSavepoint(newPoint)
				(newPoint, null)
			}
			// PROPAGATION_NESTED -- 如果当前存在事务，则在嵌套事务内执行。如果当前没有事务，则进行与PROPAGATION_REQUIRED类似的操作。
			case TS_PG_NESTED => if (null == lastPoint) {
					val newPoint = sess.connection.setSavepoint("" + System.currentTimeMillis())
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


