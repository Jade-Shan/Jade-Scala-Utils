package jadeutils.common

import jadeutils.comm.dao._

import org.scalatest.junit.JUnitRunner
import org.scalatest.FunSuite
import org.junit.runner.RunWith

@RunWith(classOf[JUnitRunner])
class SessionTest extends FunSuite with Logging {

	class TestTransaction(id: String) extends Transaction with Logging {
		private[this] var status = "ready"

		def begin() { status = "active"; logInfo("transaction {} begin", id) }
		def commit() { status = "commited"; logInfo("transaction {} commit", id) }
		def rollback() { status = "rollbacked"; logInfo("transaction {} rollback", id) }

		def isActive() = { logInfo("transaction {} status is {}", id, status); "active" == status }
	}

	class TestSession(id: String) extends DaoSession with Logging {
		var transCount = 0

		def isOpen() = "open" == status

		def open() { status = "open"; logInfo("Session {} open", id) }

		def close() { status = "closed"; logInfo("Session {} close", id) }

		def getTransaction() = if (null != trans) trans else {
			trans = new TestTransaction("" + transCount)
			transCount = transCount + 1
			trans
		}
	}

	class TestSessionFactory extends DaoSessionFactory with Logging {
		var sessCount = 0

		def createSession = {
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
			logDebug("before query")

			val u = if (id > 0) new User(id, "TestUser" + id)
			else throw new java.lang.RuntimeException("Exception for Text")

			logDebug("after query")
			u
		}

		def insert(model: User)  {
			logDebug("before insert")
			logDebug("after insert")
		}

	}

	object UserService extends TestBaseService {
		private val dao = new UserDao(getSession)

		def getUser(id: Int): User = withTransaction { dao.getById(id) }

		def insertUser(user: User) { withTransaction { dao.insert(user) } }
	}


	test("Test-Trans-commit") {
		val u = UserService.getUser(33)
//		UserService.insertUser(u)
//		logDebug("{}, {}, {}, {}, {}, {}", u)
//		assert(33 == u.id)
	}

	test("Test-Trans-rollback") {
	//	val u = UserService.getUser(-33)
	}

}
