package jadeutils.comm.dao

import java.lang.RuntimeException;

trait Transaction {
	protected var status = "ready"

	def begin(): Unit
	def commit(): Unit
	def rollback(): Unit

	def isActive: Boolean
}

trait DaoSession {
	protected var trans: Transaction = _
	protected var status = "open"

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

	def withTransaction[T](callFunc: => T)(implicit m: Manifest[T]): T = {
		warpSession(callFunc)
	}

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
			(funcResult, null)
		} catch {
			case e: RuntimeException => {
				if (getSession.getTransaction().isActive) {
					getSession.getTransaction().rollback()
					logDebug("Transaction rollback")
				}
				(generateDefaultResult(m), e)
			}
		} finally { getSession.close() }

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
