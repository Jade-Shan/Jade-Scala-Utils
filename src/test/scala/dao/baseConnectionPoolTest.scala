package jadeutils.comm.dao

import jadeutils.common.Logging

import java.sql.DriverManager
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
	val testPool = new ConnectionPool(props)

	object SqliteDaoSessionFactory extends DaoSessionFactory {
		val defaultIsolation = java.sql.Connection.TRANSACTION_SERIALIZABLE

		val pool = testPool

		def createConnection() = pool.getConnection()
	}

	class TestBaseService extends BaseTransactionService {
		val sessionFactory = SqliteDaoSessionFactory
	}

	class Employee(val id: Int, val name: String) {
		override def toString: String = "{%d, %s}".format(id, name)
	}

	class EmployeeDao(session: DaoSession) extends Dao[Employee, Int] with Logging {

		def getById(id: Int): Employee = {
			logTrace("before query")
			val u = if (id > 0) new Employee(id, "TestEmployee" + id)
			else throw new java.lang.RuntimeException("Exception for Text")
			logTrace("after query")
			u
		}

		def insert(model: Employee)  {
			logTrace("before insert")
			if (null == model) 
				throw new java.lang.RuntimeException("Exception for Text")
			logTrace("after insert")
		}

	}

	object EmployeeService extends TestBaseService {
		private val dao = new EmployeeDao(sessionFactory.currentSession)
		def getEmployee(id: Int): Employee = dao.getById(id)
		def insertEmployee(employee: Employee) { dao.insert(employee) }
	}

	test("Test-DbConnection") {
		EmployeeService.insertEmployee(new Employee(33, "TestEmployee33"))
		val u = EmployeeService.getEmployee(33)
		logInfo("{}", u)
	}

	test("Test-Pool-Size") {
		intercept[java.sql.SQLTransientConnectionException] {
			val c1 = testPool.getConnection()
			val c2 = testPool.getConnection()
			val c3 = testPool.getConnection()
			val c4 = testPool.getConnection()
			val c5 = testPool.getConnection()
			val c6 = testPool.getConnection()
		}
	}

}

