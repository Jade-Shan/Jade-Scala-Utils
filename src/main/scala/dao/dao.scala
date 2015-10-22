package jadeutils.comm.dao

import java.lang.RuntimeException
import java.sql.Connection

import jadeutils.common.Logging

class DaoSession(val id: String, val connection: Connection, 
	factory: DaoSessionFactory) extends Logging 
{
	def isBroken = connection.isClosed
	var isInTrans = false

	def close() {
		factory.closeSession(this)
		logTrace("DaoSession close: {}", id)
	}

	override def toString = "(%s, %b)".format(id, isBroken)
}

abstract class DaoSessionFactory(val minPoolSize: Int, val maxPoolSize: Int, 
	val initPoolSize: Int) extends Logging 
{ 
	def this() = this(3, 10, 5)

	private[this] val idleSess = new scala.collection.mutable.Stack[DaoSession]
	private[this] val actSesss = new scala.collection.mutable.HashMap[String, DaoSession]
	private[this] def size() = idleSess.size + actSesss.size

	def createConnection() : Connection

	private[this] val conn = new ThreadLocal[DaoSession]; 

	private[this] var session: DaoSession = _

	def currentSession = if (session != null && !session.isBroken) {
		session  
	} else {
		if (null != session)
			session.close
		createSession()
	}

	def createSession(): DaoSession = {
		logTrace("size: {} >= max: {}", size, maxPoolSize)
		if (size >= maxPoolSize) 
			throw new RuntimeException("Db connection Pool filled")

		val sess = nextSession()
		actSesss.put(sess.id, sess)
		session = sess

		logTrace("size: {} ----- max: {} idle: {} \n active: {}", 
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
		logTrace("size: {} ----- max: {} idle: {} \n active: {}", 
			size, maxPoolSize, idleSess, actSesss)
	}

}

abstract class BaseTransactionService extends Logging {

	protected val sessionFactory: DaoSessionFactory

	def withTransaction[T](callFunc: => T)(implicit m: Manifest[T]): T = {
		warpSession(callFunc)
	}

	private def warpSession[T](callFunc: => T)(implicit m: Manifest[T]): T = {
		val sess = sessionFactory.currentSession
		val conn = sess.connection
		val autoCommitBackup = conn.getAutoCommit

		if (!sess.isInTrans) {
			sess.isInTrans = true
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

	private def generateDefaultResult[T](m: Manifest[T]): Any = {
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
