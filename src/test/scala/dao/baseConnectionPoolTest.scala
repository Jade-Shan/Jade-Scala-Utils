package jadeutils.comm.dao

import jadeutils.common.Logging

import java.sql.DriverManager
import java.sql.Connection

import java.util.Properties

import org.scalatest.junit.JUnitRunner
import org.scalatest.FunSuite
import org.junit.runner.RunWith

@RunWith(classOf[JUnitRunner])
class BaseConnectionPoolTest extends FunSuite with Logging {

	val props = new Properties();
	props.setProperty("dataSourceClassName", "org.sqlite.SQLiteDataSource");
	props.setProperty("jdbcUrl", "jdbc:sqlite:db-test-03.db");
	props.setProperty("autoCommit", "false");
	props.setProperty("maximumPoolSize", "5");
	val testPool = new HikariConnectionPool(props)

	test("Test-DbConnection") {
		logDebug("======== Test Creating session =============")
		val c1 = testPool.borrow()
		val c2 = testPool.borrow()
		val c3 = testPool.borrow()
		val c4 = testPool.borrow()
		val c5 = testPool.borrow()
		assert(c1.isSuccess)
		assert(c2.isSuccess)
		assert(c3.isSuccess)
		assert(c4.isSuccess)
		assert(c5.isSuccess)
		logDebug("======== Test Closing session =============")
		testPool.retrunBack(c1)
		testPool.retrunBack(c2)
		testPool.retrunBack(c3)
		testPool.retrunBack(c4)
		testPool.retrunBack(c5)
	}

	test("Test-Pool-Size") {
		logDebug("====Test Creating session =====")
		val c1 = testPool.borrow()
		val c2 = testPool.borrow()
		val c3 = testPool.borrow()
		val c4 = testPool.borrow()
		val c5 = testPool.borrow()
		logDebug("====Test Closing session ======")
		testPool.retrunBack(c1)
		testPool.retrunBack(c2)
		testPool.retrunBack(c3)
		logDebug("====Test Open Agan ============")
		val c6 = testPool.borrow()
		val c7 = testPool.borrow()
		val c8 = testPool.borrow()
		assert(c6.isSuccess)
		assert(c7.isSuccess)
		assert(c8.isSuccess)
		logDebug("====pool should fulled ========")
		val c9 = testPool.borrow()
		assert(c9.isFailure)
//		intercept[java.sql.SQLTransientConnectionException] {
//		}
		testPool.retrunBack(c4)
		testPool.retrunBack(c5)
		testPool.retrunBack(c6)
		testPool.retrunBack(c7)
		testPool.retrunBack(c8)
	}

}

