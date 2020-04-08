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
		val c1 = testPool.getConnection()
		val c2 = testPool.getConnection()
		val c3 = testPool.getConnection()
		val c4 = testPool.getConnection()
		val c5 = testPool.getConnection()
		logDebug("======== Test Closing session =============")
		c1.close()
		c2.close()
		c3.close()
		c4.close()
		c5.close()
		logDebug("======== Test Open Agan =============")
		val c6 = testPool.getConnection()
		val c7 = testPool.getConnection()
		val c8 = testPool.getConnection()
		val c9 = testPool.getConnection()
		val ca = testPool.getConnection()
	}

	test("Test-Pool-Size") {
		logDebug("====Test Creating session =====")
		val c1 = testPool.getConnection()
		val c2 = testPool.getConnection()
		val c3 = testPool.getConnection()
		val c4 = testPool.getConnection()
		val c5 = testPool.getConnection()
		logDebug("====Test Closing session ======")
		c1.close()
		c2.close()
		c3.close()
		logDebug("====Test Open Agan ============")
		val c6 = testPool.getConnection()
		val c7 = testPool.getConnection()
		val c8 = testPool.getConnection()
		logDebug("====pool should fulled ========")
		intercept[java.sql.SQLTransientConnectionException] {
			val c9 = testPool.getConnection()
		}
	}

}

