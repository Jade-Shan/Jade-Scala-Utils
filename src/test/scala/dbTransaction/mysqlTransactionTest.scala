package jadeutils.comm.dao

import java.sql.DriverManager
import java.sql.Connection

import jadeutils.common.Logging
import jadeutils.common.EnvPropsComponent

import org.scalatest.junit.JUnitRunner
import org.scalatest.FunSuite
import org.junit.runner.RunWith
import scala.util.Try
import scala.util.Success
import scala.util.Failure

class TestMysqlService extends BaseTransactionService {
	
  def transactionName(): String = "TestMysqlService"

  val dataSource: DataSourcetHolder = MysqlDataSourceHolder
}

object UserMysqlService extends TestMysqlService {

	private val dao = new MysqlImplTestDao(dataSource)

	/* 纯读取，不需要事务 */
	def getUser(id: String): Try[Option[User]] = dao.getById(id) 

	def insertUser(user: User) = withTransaction { dao.insert(user) }

	def insertUserList(userlist: List[User]) = withTransaction {
		val ll = for (u <- userlist) yield dao.insert(u)
		logDebug("list insert result: {}", ll)
		val el = ll.filter(_.isFailure).map(_.failed.get)
		logDebug("error list is : {}", el)
		val sl = ll.filter(_.isSuccess).map(_.get)
		logDebug("success list is : {}", sl)
		if (el.size > 0) Failure(el(0)) else Success(sl)
	}

	def insertUserList2(userlist: List[User], user: User) =
		withTransaction(TransNesting.TS_PG_REQUIRED) {
			val ll = for (u <- userlist) yield dao.insert(u)
			logDebug("list insert result: {}", ll)
			val el = ll.filter(_.isFailure).map(_.failed.get)
			logDebug("error list is : {}", el)
			val sl = ll.filter(_.isSuccess).map(_.get)
			logDebug("success list is : {}", sl)
			if (el.size > 0) Failure(el(0)) else Success(sl)
			withTransaction(TransNesting.TS_PG_REQUIRED){ dao.insert(user) }
		}

	def insertUserList3(userlist: List[User], user: User) =
		withTransaction(TransNesting.TS_PG_REQUIRED) {
			val ll = for (u <- userlist) yield dao.insert(u)
			logDebug("list insert result: {}", ll)
			val el = ll.filter(_.isFailure).map(_.failed.get)
			logDebug("error list is : {}", el)
			val sl = ll.filter(_.isSuccess).map(_.get)
			logDebug("success list is : {}", sl)
			if (el.size > 0) Failure(el(0)) else Success(sl)
			withTransaction(TransNesting.TS_PG_NESTED){ dao.insert(user) }
		}
}

@RunWith(classOf[JUnitRunner])
class MysqlTransactionTest extends FunSuite with Logging {
	val testEnv = MysqlEnv
	val testService = UserMysqlService

	test("Test-00-trans-trans-commit") {
		testEnv.testInEnv(() => {
			logInfo("------------------------test auto commit\n")
			val user = new User("1", "jade")
			testService.insertUser(user)
			val result = testService.getUser(user.id)
			assert(result.isSuccess)
			assert(result.get.isDefined)
			val user1 = result.get.get
			logInfo("--------userid {} is {}", user1.id, user1)
			assert("jade" == user1.name)
		})
	} 

	test("Test-01-trans-rollback-by-exception") {
		testEnv.testInEnv(() => {
			logInfo("------------------------test rollback by exception\n")
			testService.insertUserList(new User("1", "jade") ::
					new User("2", "yun") :: new User("3", "wendy") ::
					new User("4", "wen") :: //
					new User(null, "tiantian") // id is null should rollback
			:: Nil)
			assert(testService.getUser("1").get.isEmpty)
			assert(testService.getUser("2").get.isEmpty)
			assert(testService.getUser("3").get.isEmpty)
			assert(testService.getUser("4").get.isEmpty)
		})
	}

	test("Test-02-trans-pg-require-rollback-by-same-exception") {
		testEnv.testInEnv(() => {
			// 嵌套事务，内层回滚外层一起回滚
			logInfo("------------------------test rollback by exception\n")
			val userlist = new User("1", "jade") :: new User("2", "yun") :: //
				new User("3", "wendy") :: new User("4", "wen") :: Nil
			val user = new User(null, "tiantian")
			testService.insertUserList2(userlist, user)
			logDebug("user.id {} is {}", "1", testService.getUser("1"))
			logDebug("user.id {} is {}", "2", testService.getUser("2"))
			logDebug("user.id {} is {}", "3", testService.getUser("3"))
			logDebug("user.id {} is {}", "4", testService.getUser("4"))
			assert(testService.getUser("1").get.isEmpty)
			assert(testService.getUser("2").get.isEmpty)
			assert(testService.getUser("3").get.isEmpty)
			assert(testService.getUser("4").get.isEmpty)
		})
	}

	test("Test-02-trans-nested-rollback-by-exception") {
		testEnv.testInEnv(() => {
			// 嵌套事务，内层回滚外层一起回滚
			logInfo("------------------------test rollback by exception\n")
			val userlist = new User("1", "jade") :: new User("2", "yun") :: //
				new User("3", "wendy") :: new User("4", "wen") :: Nil
			val user = new User(null, "tiantian")
			testService.insertUserList3(userlist, user)
			logDebug("user.id {} is {}", "1", testService.getUser("1"))
			logDebug("user.id {} is {}", "2", testService.getUser("2"))
			logDebug("user.id {} is {}", "3", testService.getUser("3"))
			logDebug("user.id {} is {}", "4", testService.getUser("4"))
//			assert("jade"  == testService.getUser("1").get.get.name)
//			assert("yun"   == testService.getUser("2").get.get.name)
//			assert("wendy" == testService.getUser("3").get.get.name)
//			assert("wen"   == testService.getUser("4").get.get.name)
		})
	}


}
