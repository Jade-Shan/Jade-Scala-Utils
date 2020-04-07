package jadeutils.comm.dao

import java.sql.DriverManager
import java.sql.Connection

import jadeutils.common.Logging
import jadeutils.common.EnvPropsComponent

import org.scalatest.junit.JUnitRunner
import org.scalatest.FunSuite
import org.junit.runner.RunWith

object MysqlEnv {
	val dbName = "db-test-01"
	val tableName = "testuser"

	def testInEnv(opts: (Connection) => Unit) {
		val sess = MysqlDaoSessionPool.current.right.get
		val conn = sess.conn
		val stat = conn.createStatement()
		conn.prepareStatement(//
				"drop table if exists " + MysqlEnv.tableName + "" //
			).executeUpdate();
		conn.prepareStatement(//
				"CREATE TABLE `" + MysqlEnv.dbName + "`.`" + MysqlEnv.tableName + "` " + //
			"(`id` INT NOT NULL, `name` VARCHAR(45) default '', PRIMARY KEY (`id`)) " + //
			" ENGINE = InnoDB DEFAULT CHARACTER SET = utf8mb4"
		).executeUpdate();
		opts(conn)
		conn.prepareStatement(//
				"drop table if exists " + MysqlEnv.tableName + ""//
			).executeUpdate();
		sess.close();
	}
}

object MysqlDaoSessionPool extends DaoSessionPool(3, 10, 5) {
	val defaultIsolation = TransIso.TS_SERIALIZABLE

	def connectDB() = {
		Class.forName("com.mysql.jdbc.Driver")
		Right(DriverManager.getConnection(
			"jdbc:mysql://localhost:3306/" + MysqlEnv.dbName +
				"?useSSL=false&serverTimezone=UTC&characterEncoding=UTF-8", "devuser", "devuser"
		))
	}
}

class UserMysqlDao(pool: DaoSessionPool) extends Dao[User, String] with Logging {
	def session() = pool.current
	def conn() = session.right.get.conn

	def getById(id: String): Option[User] = {
		logTrace("before query")
		if (null == id)  {
			throw new RuntimeException("User id Cannot be null")
		}
		val u =  {
			val prep = conn.prepareStatement("select * from " + //
					MysqlEnv.tableName + " where id = ?;")
			prep.setString(1, id);
			val rs = prep.executeQuery()
			val rec = if (rs.next) {
				Some(new User(rs.getString("id"), rs.getString("name")))
			} else None
			logDebug("get user: {}", rec)
			rs.close
			session.right.get.close
			rec
		}
		logTrace("after query")
		u
	}

	def insert(model: User): Unit = {
		logTrace("before insert")
		val res = if (null != model && null != model.id) {
			val prep = conn.prepareStatement("insert into " + MysqlEnv.tableName + " values (?, ?);")

			prep.setString(1, model.id);
			prep.setString(2, model.name);
			prep.addBatch();

			prep.executeBatch()
			session.right.get.close
		} else throw new RuntimeException("Exception for Text")
		logTrace("after insert")
	}

}

@RunWith(classOf[JUnitRunner])
class MySqlDaoTest extends FunSuite with Logging {

	test("Test-session-pool-00-get-session") {
		MysqlEnv.testInEnv((conn) => {
			logInfo("......................... create new session\n")
			val s1 = MysqlDaoSessionPool.borrow()
			assert(s1.isRight)
			s1.right.get.close
		})
	}

	test("Test-session-pool-01-re-use-session") {
		MysqlEnv.testInEnv((conn) => {
			logInfo("......................... create new session\n")
			val s1 = MysqlDaoSessionPool.borrow()
			val s2 = MysqlDaoSessionPool.borrow()
			val s3 = MysqlDaoSessionPool.borrow()
			assert(s1.isRight)
			assert(s2.isRight)
			assert(s3.isRight)
			logInfo("......................... close session\n")
			s1.right.get.close
			s2.right.get.close
			s3.right.get.close
			logInfo("......................... re-use in pool\n")
			val s4 = MysqlDaoSessionPool.borrow()
			val s5 = MysqlDaoSessionPool.borrow()
			val s6 = MysqlDaoSessionPool.borrow()
			assert(s4.isRight)
			assert(s5.isRight)
			assert(s6.isRight)
			logInfo("......................... close again\n")
			s4.right.get.close
			s5.right.get.close
			s6.right.get.close
		})
	}

	test("Test-session-pool-02-pool-is-full") {
		MysqlEnv.testInEnv((conn) => {
			logInfo("......................... create new session\n")
			val s1 = MysqlDaoSessionPool.borrow()
			val s2 = MysqlDaoSessionPool.borrow()
			val s3 = MysqlDaoSessionPool.borrow()
			val s4 = MysqlDaoSessionPool.borrow()
			val s5 = MysqlDaoSessionPool.borrow()
			val s6 = MysqlDaoSessionPool.borrow()
			val s7 = MysqlDaoSessionPool.borrow()
			val s8 = MysqlDaoSessionPool.borrow()
			val s9 = MysqlDaoSessionPool.borrow()
			assert(s1.isRight)
			assert(s2.isRight)
			assert(s3.isRight)
			assert(s4.isRight)
			assert(s5.isRight)
			assert(s6.isRight)
			assert(s7.isRight)
			assert(s8.isRight)
			assert(s9.isRight)
			logInfo("......................... pool overfool\n")
			val sa = MysqlDaoSessionPool.borrow()
			assert(sa.isLeft && sa.left.get.getMessage == "Db connection Pool filled")
			logInfo("......................... clean up\n")
			s1.right.get.close
			s2.right.get.close
			s3.right.get.close
			s4.right.get.close
			s5.right.get.close
			s6.right.get.close
			s7.right.get.close
			s8.right.get.close
			s9.right.get.close
		})
	}


	
	test("Test-trans-00-auto-commit") {
		MysqlEnv.testInEnv((conn) => {
			logInfo("------------------------test auto commit\n")
			val dao = new UserMysqlDao(MysqlDaoSessionPool)
			val user = new User("1", "jade")
			conn.setAutoCommit(true)
			dao.insert(user)
			logInfo("--------userid {} is {}", user.id, dao.getById(user.id).get.name)
		})
	}
	
	test("Test-trans-01-manual-commit") {
		MysqlEnv.testInEnv((conn) => {
			logInfo("------------------------test manual commit\n")
			val dao = new UserMysqlDao(MysqlDaoSessionPool)
			val user = new User("1", "jade")
			conn.setAutoCommit(false)
			val savepoint = conn.setSavepoint("" + System.currentTimeMillis())
			dao.insert(user)
			if (!conn.getAutoCommit) { conn.commit(); }
			logInfo("--------userid {} is {}", user.id, dao.getById(user.id).get.name)
		})
	}

	test("Test-trans-02-rollback-manual") {
		MysqlEnv.testInEnv((conn) => {
			logInfo("------------------------test create database\n")
			val dao = new UserMysqlDao(MysqlDaoSessionPool)
			conn.setAutoCommit(false)
			dao.insert(new User("1", "jade"))
			dao.insert(new User("2", "yun"))
			if (!conn.getAutoCommit) { conn.commit(); }
			dao.insert(new User("3", "wendy"))
			dao.insert(new User("4", "wen"))
			logInfo("--------userid {} is {}", "3", dao.getById("3").get.name)
			logInfo("--------userid {} is {}", "4", dao.getById("4").get.name)
			assert("wendy"     == dao.getById("3").get.name)
			assert("wen"      == dao.getById("4").get.name)
			conn.rollback()
			dao.insert(new User("5", "tiantian"))
			//
			assert("jade"     == dao.getById("1").get.name)
			assert("yun"      == dao.getById("2").get.name)
			assert(true       == dao.getById("3").isEmpty)
			assert(true       == dao.getById("4").isEmpty)
			assert("tiantian" == dao.getById("5").get.name)
			if (!conn.getAutoCommit) { conn.commit(); }
		})
	}


	test("Test-trans-02-rollback-manual-savepoint") {
		MysqlEnv.testInEnv((conn) => {
			logInfo("------------------------test create database\n")
			val dao = new UserMysqlDao(MysqlDaoSessionPool)
			conn.setAutoCommit(false)
			dao.insert(new User("1", "jade"))
			dao.insert(new User("2", "yun"))
			if (!conn.getAutoCommit) { conn.commit(); }
			val savepoint = conn.setSavepoint("" + System.currentTimeMillis())
			dao.insert(new User("3", "wendy"))
			dao.insert(new User("4", "wen"))
			logInfo("--------userid {} is {}", "3", dao.getById("3").get.name)
			logInfo("--------userid {} is {}", "4", dao.getById("4").get.name)
			assert("wendy"     == dao.getById("3").get.name)
			assert("wen"      == dao.getById("4").get.name)
			conn.rollback(savepoint)
			dao.insert(new User("5", "tiantian"))
			//
			assert("jade"     == dao.getById("1").get.name)
			assert("yun"      == dao.getById("2").get.name)
			assert(true       == dao.getById("3").isEmpty)
			assert(true       == dao.getById("4").isEmpty)
			assert("tiantian" == dao.getById("5").get.name)
			if (!conn.getAutoCommit) { conn.commit(); }
		})
	}
	
	test("Test-trans-03-rollback-by-exception") {
		MysqlEnv.testInEnv((conn) => {
			logInfo("------------------------test rollback by exception\n")
			val dao = new UserMysqlDao(MysqlDaoSessionPool)
			conn.setAutoCommit(false)
			dao.insert(new User("1", "jade"))
			dao.insert(new User("2", "yun"))
			dao.insert(new User("3", "wendy"))
			dao.insert(new User("4", "wen"))
			assert("jade"  == dao.getById("1").get.name)
			assert("yun"   == dao.getById("2").get.name)
			assert("wendy" == dao.getById("3").get.name)
			assert("wen"    == dao.getById("4").get.name)
			//
			intercept[java.lang.RuntimeException] {
				try {
					dao.insert(new User(null, "tiantian"))
				} catch {
					case e: RuntimeException => if (!conn.getAutoCommit) {
						conn.rollback();
						throw e
					}
				}
			}
			if (!conn.getAutoCommit) { conn.commit(); }
			assert(dao.getById("1").isEmpty)
			assert(dao.getById("2").isEmpty)
			assert(dao.getById("3").isEmpty)
			assert(dao.getById("4").isEmpty)
			assert(dao.getById("5").isEmpty)
		})
	}

}


//	class TestBaseService extends BaseTransactionService {
//		val daoSessPool = MysqlDaoSessionPool
//	}


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