package jadeutils.comm.dao

import java.sql.DriverManager
import java.sql.Connection

import jadeutils.common.Logging
import jadeutils.common.EnvPropsComponent

import org.scalatest.junit.JUnitRunner
import org.scalatest.FunSuite
import org.junit.runner.RunWith

@RunWith(classOf[JUnitRunner])
class MySqlDaoTest extends FunSuite with Logging {
	import jadeutils.comm.dao.TransIso

	val dbName = "db-test-01"
	val tableName = "testuser"

	object MysqlDaoSessionPool extends DaoSessionPool (3, 10, 5) {
		val defaultIsolation = TransIso.TS_SERIALIZABLE

		def connectDB(): java.sql.Connection = {
			Class.forName("com.mysql.jdbc.Driver")
			DriverManager.getConnection(
				"jdbc:mysql://localhost:3306/" + dbName +
				"?useSSL=false&serverTimezone=UTC&characterEncoding=UTF-8","devuser","devuser")
		}
	}

	class TestBaseService extends BaseTransactionService {
		val daoSessPool = MysqlDaoSessionPool
	}

	class User(val id: String, val name: String) {
		override def toString: String = "{%s, %s}".format(id, name)
	}

	class UserDao(pool: DaoSessionPool) 
	extends Dao[User, String] with Logging 
	{
		def session() = pool.current
		def conn() = session.conn

		def getById(id: String): Either[RuntimeException, User] = {
			logTrace("before query")
			val u = if (null != id) {
				val prep = conn.prepareStatement("select * from " + tableName + " where id = ?;")
				prep.setString(1, id);
				val rs = prep.executeQuery()
				val rec = if (rs.next) {
					Right(new User(rs.getString("id"), rs.getString("name")))
				} else Left(new RuntimeException("No such Rec"))
				logDebug("get user: {}", rec)
				rs.close
				session.close
				rec
			} else throw new RuntimeException("Exception for Text")
			logTrace("after query")
			u
		}

		def insert(model: User)  {
			logTrace("before insert")
			if (null != model && null != model.id) {
				val prep = conn.prepareStatement("insert into " + tableName + " values (?, ?);")

				prep.setString(1, model.id);
				prep.setString(2, model.name);
				prep.addBatch();

				prep.executeBatch()
				session.close
			} else throw new RuntimeException("Exception for Text")
			logTrace("after insert")
		}

	}

	def testInEnv(opts: (Connection) => Unit) {
		val conn = MysqlDaoSessionPool.current.conn
		
		val stat = conn.createStatement()
		conn.prepareStatement("drop table if exists " + tableName + "").executeUpdate();
		conn.prepareStatement("CREATE TABLE `" + dbName +  "`.`" + tableName + "` " + // 
				"(`id` INT NOT NULL, `name` VARCHAR(45) default '', PRIMARY KEY (`id`)) "//
				).executeUpdate();
		opts(conn)
		conn.prepareStatement("drop table if exists " + tableName + "").executeUpdate();
		conn.close();
	}

	test("Test-trans-00") {
		testInEnv((conn) => {
			logInfo("------------------------test create database\n")
			val dao = new UserDao(MysqlDaoSessionPool)
			val user = new User("1", "jade")
			conn.setAutoCommit(false)
			val savepoint = conn.setSavepoint("" + System.currentTimeMillis())
			dao.insert(user)
			// conn.rollback()
			// conn.rollback(savepoint)
			if (!conn.getAutoCommit) { conn.commit(); }
			logInfo("--------userid {} is {}", user.id, dao.getById(user.id).right.get.name)
		})
	}

//	test("Test-trans-01") {
//		testInEnv((conn) => {
//			logInfo("......................... conn trans\n")
//
//			object UserService extends TestBaseService {
//				private val dao = new UserDao(MysqlDaoSessionPool)
//
//				def getUser(id: String): User = { dao.getById(id) }
//
//				def insertUser(user: User) { 
//					dao.conn().setAutoCommit(false)
//					dao.insert(user) 
//					dao.conn().commit()
//				}
//			}
//			val user = new User("1", "jade")
//			UserService.insertUser(user)
//		})
//	}
//
//	test("Test-trans-02") {
//		testInEnv((conn) => {
//			logInfo("......................... will commit\n")
//
//			object UserService extends TestBaseService {
//				private val dao = new UserDao(MysqlDaoSessionPool)
//
//				def getUser(id: String): User = withTransaction { dao.getById(id) }
//
//				def insertUser(user: User) { withTransaction { dao.insert(user) } }
//
//				def insertUserList(userlist: List[User]) {
//					withTransaction {
//						userlist.foreach((user) => { dao.insert(user) })
//					}
//				}
//			}
//			val user = new User("1", "jade")
//			UserService.insertUser(user)
//			val u1 = UserService.getUser("1")
//			assert("1" == u1.id && "jade" == u1.name)
//
//			UserService.insertUserList(new User("2", "yun") ::
//				new User("3", "wendy") :: new User("4", "wen") ::
//				new User("5", "tiantian") :: Nil)
//			val u2 = UserService.getUser("2")
//			val u3 = UserService.getUser("3")
//			val u4 = UserService.getUser("4")
//			val u5 = UserService.getUser("5")
//			assert("2" == u2.id && "yun" == u2.name)
//			assert("3" == u3.id && "wendy" == u3.name)
//			assert("4" == u4.id && "wen" == u4.name)
//			assert("5" == u5.id && "tiantian" == u5.name)
//		})
//	}
//
//	test("Test-trans-rollback") {
//		testInEnv((conn) => {
//			logInfo("......................... will rollback\n")
//
//			object UserService extends TestBaseService {
//				private val dao = new UserDao(MysqlDaoSessionPool)
//
//				def getUser(id: String): User = withTransaction { dao.getById(id) }
//
//				def insertUser(user: User) { withTransaction { dao.insert(user) } }
//
//				def insertUserList(userlist: List[User]) {
//					withTransaction {
//						userlist.foreach((user) => { dao.insert(user) })
//					}
//				}
//			}
//			intercept[java.lang.Exception] {
//				UserService.insertUserList(new User("1", "jade") ::
//					new User("2", "yun") :: new User("3", "wendy") ::
//					new User("4", "wen") :: new User(null, "tiantian") :: Nil)
//			}
//
//			assert(null == UserService.getUser("1"))
//			assert(null == UserService.getUser("2"))
//			assert(null == UserService.getUser("3"))
//			assert(null == UserService.getUser("4"))
//			assert(null == UserService.getUser("5"))
//		})
//	}

}
