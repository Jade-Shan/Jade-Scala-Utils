package jadeutils.comm.dao

import java.lang.RuntimeException
import java.sql.Connection

import jadeutils.common.Logging

// PROPAGATION_REQUIRED -- 支持当前事务，如果当前没有事务，就新建一个事务。这是最常见的选择。
// PROPAGATION_SUPPORTS -- 支持当前事务，如果当前没有事务，就以非事务方式执行。
// PROPAGATION_MANDATORY -- 支持当前事务，如果当前没有事务，就抛出异常。
// PROPAGATION_REQUIRES_NEW -- 新建事务，如果当前存在事务，把当前事务挂起。
// PROPAGATION_NOT_SUPPORTED -- 以非事务方式执行操作，如果当前存在事务，就把当前事务挂起。
// PROPAGATION_NEVER -- 以非事务方式执行，如果当前存在事务，则抛出异常。
// PROPAGATION_NESTED -- 如果当前存在事务，则在嵌套事务内执行。如果当前没有事务，则进行与PROPAGATION_REQUIRED类似的操作。 
object TransNesting extends Enumeration {
	type TransNesting = Value
	val REQUIRED      = Value(0, "REQUIRED")
	val SUPPORTS      = Value(1, "SUPPORTS")
	val MANDATORY     = Value(2, "MANDATORY")
	val REQUIRES_NEW  = Value(3, "REQUIRES_NEW")
	val NOT_SUPPORTED = Value(4, "NOT_SUPPORTED")
  val NEVER         = Value(5, "NEVER")
  val NESTED        = Value(6, "NESTED")
}

class DaoSession(val id: String, val connection: Connection, 
	factory: DaoSessionFactory) extends Logging 
{
	def isBroken = connection.isClosed
	var isInTrans = false

	def close() { factory.closeSession(this) }

	override def toString = "(%s, %b)".format(id, isBroken)
}

abstract class DaoSessionFactory(val minPoolSize: Int, val maxPoolSize: Int, 
	val initPoolSize: Int) extends Logging 
{ 
	val defaultIsolation: Int

	private[this] val idleSess = new scala.collection.mutable.Stack[DaoSession]
	private[this] val actSesss = new scala.collection.mutable.HashMap[String, DaoSession]
	private[this] def size() = idleSess.size + actSesss.size
	private[this] var currSession = new ThreadLocal[DaoSession]

	def this() = this(3, 10, 5)

	def createConnection() : Connection

	def currentSession = if ( currSession.get != null && 
		!currSession.get.isBroken)
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
		actSesss.put(sess.id, sess)
		currSession.set(sess)

		logTrace(
			"after create session: size: {} ----- max: {}\nidle: {}\nactive: {}", 
			size, maxPoolSize, idleSess, actSesss)
		sess
	}

	private[this] def nextSession(): DaoSession = {
		val sess  = if (idleSess.size < 1) {
			new DaoSession("" + size, createConnection(), this)
		} else idleSess.pop

		if (sess.isBroken) {
			nextSession()  // drop borken session, find next idle session
		} else sess
	}

	def closeSession(sess: DaoSession) {
		if (actSesss.contains(sess.id)) {
			actSesss.remove(sess.id)
			idleSess.push(sess)
		}
		logTrace(
			"after close session: size: {} ----- max: {}\nidle: {}\nactive: {}", 
			size, maxPoolSize, idleSess, actSesss)
	}

}

abstract class BaseTransactionService extends Logging {
	import jadeutils.comm.dao.TransNesting.TransNesting

	protected val sessionFactory: DaoSessionFactory

	def withTransaction[T](nesting: TransNesting, iso: Int, 
		callFunc: => T)(implicit m: Manifest[T]): T = 
	{ warpSession(nesting, iso, callFunc) }

	def withTransaction[T](nesting: TransNesting, callFunc: => T)
	(implicit m: Manifest[T]): T = 
	{ warpSession(nesting, sessionFactory.defaultIsolation, callFunc) }

	def withTransaction[T](iso: Int, 
		callFunc: => T)(implicit m: Manifest[T]): T = 
	{ warpSession(TransNesting.REQUIRED, iso, callFunc) }

	def withTransaction[T](callFunc: => T)(implicit m: Manifest[T]): T = {
		warpSession(TransNesting.REQUIRED, sessionFactory.defaultIsolation, 
			callFunc)
	}

	private def warpSession[T](nesting: TransNesting, iso: Int, 
		callFunc: => T)(implicit m: Manifest[T]): T = 
	{
		val sess = sessionFactory.currentSession
		val conn = sess.connection
		val autoCommitBackup = conn.getAutoCommit

		if (!sess.isInTrans) {
			sess.isInTrans = true
			conn.setTransactionIsolation(iso)
			// dealwithTransNesting(conn, nesting)
			conn.setAutoCommit(false)
			logTrace("Trans begin: S: {}", sess.id)
		}

		var result = try {
			var funcResult = callFunc
			if (sess.isInTrans) {
				conn.commit()
				logTrace("Trans commit: S: {}", sess.id)
			}
			(funcResult, null)
		} catch {
			case e: RuntimeException => {
				if (sess.isInTrans) {
					conn.rollback()
					logTrace("Trans rollback: S: {}", sess.id)
				}
				(generateDefaultResult(m), e)
			}
		} finally {
			conn.setAutoCommit(autoCommitBackup)
			sess.isInTrans = false
			sess.close
			logTrace("Trans end: S: {}", sess.id)
		}

		if (null != result._2) throw result._2

		result._1.asInstanceOf[T]
	}

//	private[this] def dealwithTransNesting(conn: Connection, nesting: TransNesting) {
//		nesting match {
//			case TransNesting.REQUIRED      => {}
//			case TransNesting.SUPPORTS      => {}
//			case TransNesting.MANDATORY     => {}
//			case TransNesting.REQUIRES_NEW  => {}
//			case TransNesting.NOT_SUPPORTED => {}
//			case TransNesting.NEVER         => {}
//			case TransNesting.NESTED        => {}
//			case _ => logError("Unknow Trans Prop")
//		}
//	}

	private[this] def generateDefaultResult[T](m: Manifest[T]): Any = {
		if (m <:< manifest[Byte]) 0
		else if (m <:< manifest[Short]) 0
		else if (m <:< manifest[Int]) 0
		else if (m <:< manifest[Long]) 0L
		else if (m <:< manifest[Float]) 0F
		else if (m <:< manifest[Double]) 0
		else if (m <:< manifest[Char]) '0'
		else if (m <:< manifest[Boolean]) false
		else if (m <:< manifest[Unit]) { () }
		else null
	}

}

trait Dao[T, K] {

	def getById(id: K): T
	def insert(model: T): Unit
}
