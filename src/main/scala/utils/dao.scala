package jadeutils.comm.dao

import java.lang.RuntimeException;

trait Transaction {

	def begin(): Unit
	def commit(): Unit
	def rollback(): Unit

	def isActive: Boolean
}

trait DaoSession {
	var trans: Transaction = _

	var status = "open"

	def isOpen(): Boolean

	def open(): Unit

	def close(): Unit

	def getTransaction(): Transaction
}

trait DaoSessionFactory {
	var session: DaoSession = _

	def currentSession = session

	def createSession(): DaoSession

}

abstract class BasicDao[T, K](session: DaoSession) {

	def getById(id: K): T

	def insert(model: T): Unit

}

trait DaoSessionFactoryHelper {

	var _session: DaoSession = _

	def initSessionFactory(): DaoSessionFactory

	lazy val sessionFactory = initSessionFactory()

	def getSession = if (_session != null && _session.isOpen())
		_session else sessionFactory.createSession()

}

abstract class BaseTransactionService extends jadeutils.common.Logging {

	val sfHelper: DaoSessionFactoryHelper

	def getSession = sfHelper.getSession

	def withTransaction[T](callFunc: => T)(implicit m: Manifest[T]): T = warpSession(callFunc)

	private def warpSession[T](callFunc: => T)(implicit m: Manifest[T]): T = {
		if (!getSession.getTransaction().isActive) {
			getSession.getTransaction().begin()
			logDebug("Transaction begin")
		}

		var result = try {
			var funcResult = callFunc
			if (getSession.getTransaction().isActive) {
				getSession.getTransaction().commit()
				logDebug("Transaction commit")
			}
			funcResult
		} catch {
			case e: RuntimeException => {
				if (getSession.getTransaction().isActive) {
					getSession.getTransaction().rollback()
					logDebug("Transaction rollback")
				}
				throw e
			}
		} finally { getSession.close() }

		
		
		result.asInstanceOf[T]
	}

}
