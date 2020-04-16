package jadeutils.comm.dao

import org.scalatest.junit.JUnitRunner
import org.scalatest.FunSuite
import org.junit.runner.RunWith

import jadeutils.common.Logging
import jadeutils.common.EnvPropsComponent

import java.sql.DriverManager
import java.sql.Connection
import java.util.Properties
import scala.util.Try
import scala.util.Failure
import scala.util.Success
import java.util.Date

class SqliteImplTestDao(dataSource: DataSourcetHolder) //
extends JDBCTemplateDao[User, String](dataSource) with Logging {
	
}

@RunWith(classOf[JUnitRunner])
class SqliteDaoImplTest extends FunSuite with Logging {
	val dao = new SqliteImplTestDao(SqliteDataSourceHolder)

	test("Test-dao-impl-test-00-sql-inset-get") {
		SqliteEnv.testInEnv(() => {
			val now = new Date(System.currentTimeMillis())
			val user = new User("1", "Jade", now, now)
			dao.insert(user)
			logDebug("------------after insert")
			val u = dao.getById("1")
			logDebug("------------after load: {}", u)
			assert(u.isSuccess)
			assert(u.get.isDefined)
			assert(u.get.get.id == "1")
			assert(u.get.get.name == "Jade")
			logDebug("all : {}", dao.query("select * from testuser where 1=1"))
		})
	}

	test("Test-dao-impl-test-01-sql-inset-update") {
		SqliteEnv.testInEnv(() => {
			val now = new Date(System.currentTimeMillis())
			val user = new User("1", "Jade", now, now)
			dao.insert(user)
			logDebug("------------after insert")
			val u = dao.getById("1")
			logDebug("------------after load: {}", u)
			assert(u.isSuccess)
			assert(u.get.isDefined)
			assert(u.get.get.id == "1")
			assert(u.get.get.name == "Jade")

			user.name = "Jade Update"
			dao.update(user)
			logDebug("------------after update")
			val v = dao.getById("1")
			logDebug("------------after load: {}", v)
			assert(v.isSuccess)
			assert(u.get.isDefined)
			assert(v.get.get.id == "1")
			assert(v.get.get.name == "Jade Update")
		})
	}

	test("Test-dao-impl-test-02-sql-inset-or-update") {
		SqliteEnv.testInEnv(() => {
			val now = new Date(System.currentTimeMillis())
			val user = new User("1", "Jade", now, now)
			dao.insert(user)
			logDebug("------------after insert")
			val u = dao.getById("1")
			logDebug("------------after load: {}", u)
			assert(u.isSuccess)
			assert(u.get.isDefined)
			assert(u.get.get.id == "1")
			assert(u.get.get.name == "Jade")

			user.name = "Jade Update"
			dao.insertOrUpdate(user)
			logDebug("------------after update")
			val v = dao.getById("1")
			logDebug("------------after load: {}", v)
			assert(v.isSuccess)
			assert(u.get.isDefined)
			assert(v.get.get.id == "1")
			assert(v.get.get.name == "Jade Update")
		})
	}
}
