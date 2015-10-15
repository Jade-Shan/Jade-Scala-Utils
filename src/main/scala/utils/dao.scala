package jadeutils.comm.dao

trait Transaction {
	var status = "ready"

	def begin(): Unit
	def commit(): Unit
	def rollback(): Unit

	def wasCommitted: Boolean
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

	def withTransaction[T](f: => T): T = warpSession(f)

	private def warpSession[T](f: => T): T = {
		if(getSession.getTransaction().wasCommitted)
			getSession.getTransaction().begin()
		logDebug("Transaction begin")

		val t = f
		if(getSession.getTransaction().isActive)
			getSession.getTransaction().commit()
		logDebug("Transaction commit")

		getSession.close()

		t
	}

}
