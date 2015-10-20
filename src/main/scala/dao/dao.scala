package jadeutils.comm.dao

import java.lang.RuntimeException;

trait Transaction {
	protected var status = "ready"

	def getId(): String
	def isActive(): Boolean

	def begin(): Unit
	def commit(): Unit
	def rollback(): Unit
}

trait DaoSession {
	protected var trans: Transaction = _
	protected var status = "open"

	def getId(): String
	def isOpen(): Boolean
	def isAutoCommit(): Boolean
	def getTransaction(): Transaction

	def close(): Unit
	def open(): Unit
	def setAutoCommit(isAuto: Boolean): Unit
}

trait DaoSessionFactory {
	var session: DaoSession = _

	def currentSession = session
	def createSession(): DaoSession
}

trait DaoSessionFactoryHelper {
	var session: DaoSession = _
	lazy val sessionFactory = initSessionFactory()

	def initSessionFactory(): DaoSessionFactory

	def getSession = if (session != null && session.isOpen()) {
		session  
	} else sessionFactory.createSession()

}

abstract class BaseTransactionService extends jadeutils.common.Logging {

	val sfHelper: DaoSessionFactoryHelper

	protected def getSession() = sfHelper.getSession

	def withTransaction[T](callFunc: => T)(implicit m: Manifest[T]): T = {
		warpSession(callFunc)
	}

	private def warpSession[T](callFunc: => T)(implicit m: Manifest[T]): T = {
		val autoCommitBackup = getSession.isAutoCommit
		val trans = getSession.getTransaction

		if (!trans.isActive) {
			trans.begin()
			logTrace("Trans begin: S: {} T: {}", getSession.getId, trans.getId)
		}

		var result = try {
			var funcResult = callFunc
			if (trans.isActive) {
				trans.commit()
				logTrace("Trans commit: S: {} T: {}", getSession.getId, trans.getId)
			}
			(funcResult, null)
		} catch {
			case e: RuntimeException => {
				if (trans.isActive) {
					trans.rollback()
					logTrace("Trans rollback: S: {} T: {}", getSession.getId, trans.getId)
				}
				(generateDefaultResult(m), e)
			}
		} finally {
			getSession.setAutoCommit(autoCommitBackup)
			getSession.close()
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
		else if (m <:< manifest[Unit]) ()
			else null
	}

}

traic Dao[T, K] {
	def getById(id: K): T
	def insert(model: T): Unit
}
