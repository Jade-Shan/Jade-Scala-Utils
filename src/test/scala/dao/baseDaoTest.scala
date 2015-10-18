package jadeutils.comm.dao

import jadeutils.common._

import org.scalatest.junit.JUnitRunner
import org.scalatest.FunSuite
import org.junit.runner.RunWith

@RunWith(classOf[JUnitRunner])
class SessionTest extends FunSuite with Logging {

	class TestTransaction(val id: String) extends Transaction with Logging {
		def getId = id
		def isActive = "active" == status

		def begin() { status = "active" }
		def commit() { status = "commited" }
		def rollback() { status = "rollbacked" }
	}

	class TestSession(id: String) extends DaoSession with Logging {
		private[this] var transCount = 0
		private[this] var autoCommit = true

		def getId = id
		def isOpen = "open" == status
		def isAutoCommit = autoCommit
		def getTransaction() = if (null != trans) trans else {
			trans = new TestTransaction("" + transCount)
			transCount = transCount + 1
			trans
		}

		def open()  { status = "open";   logTrace("Session {} open", id) }
		def close() { status = "closed"; logTrace("Session {} close", id) }
		def setAutoCommit(isAuto: Boolean) { autoCommit = isAuto }
	}

	class TestSessionFactory extends DaoSessionFactory with Logging {
		private[this] var sessCount = 0

		def createSession = if (null != session) session else {
			session = new TestSession("" + sessCount)
			sessCount = sessCount + 1
			session
		}
	}

	object TestSessionFactoryHelper extends DaoSessionFactoryHelper {

		def initSessionFactory = new TestSessionFactory

	}

	class TestBaseService extends BaseTransactionService {
		val sfHelper = TestSessionFactoryHelper
	}

	class User(val id: Int, val name: String) {
		override def toString: String = "{%d, %s}".format(id, name)
	}

	class UserDao(session: DaoSession) extends BasicDao[User, Int](session) 
		with Logging
	{

		def getById(id: Int): User = {
			logTrace("before query")

			val u = if (id > 0) new User(id, "TestUser" + id)
			else throw new java.lang.RuntimeException("Exception for Text")

			logTrace("after query")
			u
		}

		def insert(model: User)  {
			logTrace("before insert")
			if (null == model) 
				throw new java.lang.RuntimeException("Exception for Text")
			logTrace("after insert")
		}

	}

	object UserService extends TestBaseService {
		private val dao = new UserDao(getSession)

		def getUser(id: Int): User = withTransaction { dao.getById(id) }
		def insertUser(user: User) { withTransaction { dao.insert(user) } }
	}


	test("Test-Trans-get-commit") {
		logInfo("======== test get commit =============")
		val u = UserService.getUser(33)
		logInfo("{}", u)
	}

	test("Test-Trans-insert-commit") {
		logInfo("======== test insert commit =============")
		UserService.insertUser(new User(33, "Testuser33"))
	}

	test("Test-Trans-get-rollback") {
		logInfo("======== test get rollback =============")
		intercept[java.lang.Exception] {
			val u = UserService.getUser(-33)
			logInfo("{}", u)
		}
	}

	test("Test-Trans-insert-rollback") {
		logInfo("======== test insert rollback =============")
		intercept[java.lang.Exception] {
			UserService.insertUser(null)
		}
	}

}
