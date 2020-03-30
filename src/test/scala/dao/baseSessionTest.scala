package jadeutils.comm.dao

import jadeutils.common.Logging

import java.sql.DriverManager

import org.scalatest.junit.JUnitRunner
import org.scalatest.FunSuite
import org.junit.runner.RunWith

@RunWith(classOf[JUnitRunner])
class SessionTest extends FunSuite with Logging {
	import jadeutils.comm.dao.TransIso

	object SqliteDaoSessionFactory extends DaoSessionPool(3, 10, 5) {
		val defaultIsolation = TransIso.TS_SERIALIZABLE

		def connectDB() = DriverManager.getConnection(
			"jdbc:sqlite:db-test-00.db")
	}

	class TestBaseService extends BaseTransactionService {
		val daoSessPool = SqliteDaoSessionFactory
	}

	class User(val id: Int, val name: String) {
		override def toString: String = "{%d, %s}".format(id, name)
	}

	class UserDao(session: DaoSession) extends Dao[User, Int] with Logging {

		def getById(id: Int): Either[RuntimeException, User] = {
			logTrace("before query")
			val u = if (id > 0) {
				Right(new User(id, "TestUser" + id))
			} else Left(new RuntimeException("Exception for Text"))
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

//	object UserService extends TestBaseService {
//		private val dao = new UserDao(daoSessPool.current)
//		def getUser(id: Int): User = withTransaction { dao.getById(id) }
//		def insertUser(user: User) { withTransaction { dao.insert(user) } }
//	}


	test("Test-session-pool") {
		val fc = SqliteDaoSessionFactory
		logInfo("......................... create new session\n")
		val s1 = fc.create()
		val s2 = fc.create()
		val s3 = fc.create()
		val s4 = fc.create()
		val s5 = fc.create()
		val s6 = fc.create()
		logInfo("......................... close session\n")
		s1.close
		s2.close
		s3.close
		logInfo("......................... re-use in pool\n")
		val s7 = fc.create()
		val s8 = fc.create()
		val s9 = fc.create()
		logInfo("......................... full pool\n")
		val sa = fc.create()
		val sb = fc.create()
		val sc = fc.create()
		val sd = fc.create()
		logInfo("......................... pool overfool\n")
		intercept[java.lang.Exception] {
			val ce = fc.create()
		}
		logInfo("......................... clean up\n")
		s4.close
		s5.close
		s6.close
		s7.close
		s8.close
		s9.close
		sa.close
		sb.close
		sc.close
		sd.close
	}

//	test("Test-Trans-get-commit") {
//		logInfo("======== test get commit =============")
//		val u = UserService.getUser(33)
//		logInfo("{}", u)
//	}
//
//	test("Test-Trans-insert-commit") {
//		logInfo("======== test insert commit =============")
//		UserService.insertUser(new User(33, "Testuser33"))
//	}
//
//	test("Test-Trans-get-rollback") {
//		logInfo("======== test get rollback =============")
//		intercept[java.lang.Exception] {
//			val u = UserService.getUser(-33)
//			logInfo("{}", u)
//		}
//	}
//
//	test("Test-Trans-insert-rollback") {
//		logInfo("======== test insert rollback =============")
//		intercept[java.lang.Exception] {
//			UserService.insertUser(null)
//		}
//	}

}
