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

class SqliteImplTestDao(dataSource: DataSourcetHolder) extends JDBCTemplateDao[User, String](dataSource) with Logging {
	
  def insert(model: User): Try[Unit] = { ??? }

  def update(model: User): Try[Unit] = { ??? }
}

@RunWith(classOf[JUnitRunner])
class SqliteDaoImplTest extends FunSuite with Logging {

	test("Test-dao-impl-test-00-sql-inset-get") {
		SqliteEnv.testInEnv(() => {
		})
	}

}
