package jadeutils.comm.dao

import jadeutils.common.Logging
import java.sql.DriverManager
import org.scalatest.junit.JUnitRunner
import org.scalatest.FunSuite
import org.junit.runner.RunWith

@RunWith(classOf[JUnitRunner])
class SqliteDaoSessionTest extends FunSuite with Logging {

	test("Test-00-session-pool-size") {
		val fc = SqliteDaoSessionPool
		logInfo("......................... create new session\n")
		val s1 = fc.borrow()
		val s2 = fc.borrow()
		val s3 = fc.borrow()
		val s4 = fc.borrow()
		val s5 = fc.borrow()
		val s6 = fc.borrow()
		logInfo("......................... close session\n")
		s1.right.get.close
		s2.right.get.close
		s3.right.get.close
		logInfo("......................... re-use in pool\n")
		val s7 = fc.borrow()
		val s8 = fc.borrow()
		val s9 = fc.borrow()
		logInfo("......................... full pool\n")
		val sa = fc.borrow()
		val sb = fc.borrow()
		val sc = fc.borrow()
		val sd = fc.borrow()
		logInfo("......................... pool overfool\n")
		val ce = fc.borrow()
		assert(ce.isLeft && ce.left.get.getMessage == "Db connection Pool filled")
		logInfo("......................... clean up\n")
		s4.right.get.close
		s5.right.get.close
		s6.right.get.close
		s7.right.get.close
		s8.right.get.close
		s9.right.get.close
		sa.right.get.close
		sb.right.get.close
		sc.right.get.close
		sd.right.get.close
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
