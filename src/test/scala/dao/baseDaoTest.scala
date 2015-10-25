package jadeutils.comm.dao

import jadeutils.common.Logging

import java.sql.DriverManager

import org.scalatest.junit.JUnitRunner
import org.scalatest.FunSuite
import org.junit.runner.RunWith

@RunWith(classOf[JUnitRunner])
class SessionTest extends FunSuite with Logging {

	object SqliteDaoSessionFactory extends DaoSessionFactory {
		val defaultIsolation = TransIso.SERIALIZABLE

		def createConnection() = DriverManager.getConnection(
			"jdbc:sqlite:test.db")
	}

	class TestBaseService extends BaseTransactionService {
		val sessionFactory = SqliteDaoSessionFactory
	}

	class User(val id: Int, val name: String) {
		override def toString: String = "{%d, %s}".format(id, name)
	}

	class UserDao(session: DaoSession) extends Dao[User, Int] with Logging {

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
		private val dao = new UserDao(sessionFactory.currentSession)
		def getUser(id: Int): User = withTransaction { dao.getById(id) }
		def insertUser(user: User) { withTransaction { dao.insert(user) } }
	}


	test("Test-session-pool") {
		val fc = SqliteDaoSessionFactory
		logInfo("......................... create new session\n")
		val c1 = fc.createSession()
		val c2 = fc.createSession()
		val c3 = fc.createSession()
		val c4 = fc.createSession()
		val c5 = fc.createSession()
		val c6 = fc.createSession()
		logInfo("......................... close session\n")
		c1.close
		c2.close
		c3.close
		logInfo("......................... re-use in pool\n")
		val c7 = fc.createSession()
		val c8 = fc.createSession()
		val c9 = fc.createSession()
		logInfo("......................... full pool\n")
		val ca = fc.createSession()
		val cb = fc.createSession()
		val cc = fc.createSession()
		val cd = fc.createSession()
		logInfo("......................... pool overfool\n")
		intercept[java.lang.Exception] {
			val ce = fc.createSession()
		}
		logInfo("......................... clean up\n")
		c4.close
		c5.close
		c6.close
		c7.close
		c8.close
		c9.close
		ca.close
		cb.close
		cc.close
		cd.close
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
