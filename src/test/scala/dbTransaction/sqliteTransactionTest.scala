package jadeutils.comm.dao

import java.sql.DriverManager
import java.sql.Connection

import jadeutils.common.Logging
import jadeutils.common.EnvPropsComponent

import org.scalatest.junit.JUnitRunner
import org.scalatest.FunSuite
import org.junit.runner.RunWith

class TestSqliteService extends BaseTransactionService {
	
  def transactionName(): String = "TestSqliteService"

  val dataSource: DataSourcetHolder = SqliteDataSourceHolder
}

object UserSqliteService extends TestSqliteService {

	private val dao = new UserSqliteDao(SqliteDataSourceHolder)

	def getUser(id: String): Option[User] = withTransaction { dao.getById(id) }

	def insertUser(user: User) { withTransaction { dao.insert(user) } }

	def insertUserList(userlist: List[User]) {
		withTransaction {
			userlist.foreach((user) => { dao.insert(user) })
		}
	}
}

@RunWith(classOf[JUnitRunner])
class SqliteTransactionTest extends FunSuite with Logging {

	test("Test-trans-00-trans-commit") {
		SqliteEnv.testInEnv(() => {
//			logInfo("------------------------test auto commit\n")
//			val user = new User("1", "jade")
//			UserSqliteService.insertUser(user)
//			logInfo("--------userid {} is {}", user.id, //
//					UserSqliteService.getUser(user.id).get)
//			assert("jade"     == UserSqliteService.getUser(user.id).get.name)
		})
	} 

	test("Test-trans-01-rollback-by-exception") {
		SqliteEnv.testInEnv(() => {
//			logInfo("------------------------test rollback by exception\n")
//			UserSqliteService.insertUserList(new User("1", "jade") ::
//					new User("2", "yun") :: new User("3", "wendy") ::
//					new User("4", "wen") :: new User(null, "tiantian") :: Nil)
////			UserSqliteService.insertUser(new User("1", "jade"))
////			UserSqliteService.insertUser(new User("2", "yun"))
////			UserSqliteService.insertUser(new User("3", "wendy"))
////			UserSqliteService.insertUser(new User("4", "wen"))
////			logInfo("--------userid {} is {}", "1", UserSqliteService.getUser("1").get)
////			logInfo("--------userid {} is {}", "2", UserSqliteService.getUser("2").get)
////			logInfo("--------userid {} is {}", "3", UserSqliteService.getUser("3").get)
////			logInfo("--------userid {} is {}", "4", UserSqliteService.getUser("4").get)
////			assert("jade"  == UserSqliteService.getUser("1").get.name)
////			assert("yun"   == UserSqliteService.getUser("2").get.name)
////			assert("wendy" == UserSqliteService.getUser("3").get.name)
////			assert("wen"    == UserSqliteService.getUser("4").get.name)
////			UserSqliteService.insertUser(new User(null, "tiantian"))
//			assert(UserSqliteService.getUser("1").isEmpty)
//			assert(UserSqliteService.getUser("2").isEmpty)
//			assert(UserSqliteService.getUser("3").isEmpty)
//			assert(UserSqliteService.getUser("4").isEmpty)
		})
	}


}
