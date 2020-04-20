package net.jadedungeon.scalautil.dao

import java.sql.DriverManager
import java.sql.Connection

import net.jadedungeon.scalautil.common.Logging
import net.jadedungeon.scalautil.common.EnvPropsComponent

import org.scalatest.junit.JUnitRunner
import org.scalatest.FunSuite
import org.junit.runner.RunWith
import java.util.Properties
import scala.util.Try
import scala.util.Success
import scala.util.Failure

import net.jadedungeon.scalautil.dao.DialectMySQL.{dialect => MySQLDialect}

object MysqlEnv extends Logging {
	val dbName = "db-test-01"
	val tableName = "testuser"
	val dbProps = new Properties();
	dbProps.setProperty("driverClassName", "com.mysql.cj.jdbc.Driver");
	dbProps.setProperty("jdbcUrl", "jdbc:mysql://localhost:3306/" + dbName
			+ "?useSSL=false&serverTimezone=UTC&characterEncoding=UTF-8");
	dbProps.setProperty("username", "devuser");
	dbProps.setProperty("password", "devuser");
	dbProps.setProperty("autoCommit", "true");
	dbProps.setProperty("maximumPoolSize", "10");


	def testInEnv(opts: () => Unit) {
		logDebug("----------------before init test table")
		val conn = MysqlDataSourcePool.borrow().get
		conn.setAutoCommit(true)
		conn.prepareStatement(//
				"drop table if exists " + MysqlEnv.tableName + "" //
			).executeUpdate();
		conn.prepareStatement(//
				"CREATE TABLE `" + MysqlEnv.dbName + "`.`" + MysqlEnv.tableName + "` " + //
			"(`id` VARCHAR(45), `name` VARCHAR(45) default '', " + //
			" `create_time` TIMESTAMP DEFAULT CURRENT_TIMESTAMP, " + //
			" `last_change_time` TIMESTAMP DEFAULT CURRENT_TIMESTAMP " + //
			" ON UPDATE CURRENT_TIMESTAMP, PRIMARY KEY (`id`)) " + //
			" ENGINE = InnoDB DEFAULT CHARACTER SET = utf8mb4 "
		).executeUpdate();
		// if (!conn.getAutoCommit) conn.commit()
		MysqlDataSourcePool.retrunBack(conn)
		logDebug("----------------after init test table")
		opts()
		logDebug("----------------before clean test table")
		val conn2 = MysqlDataSourcePool.borrow().get
		conn2.setAutoCommit(true)
		conn2.prepareStatement(//
				"drop table if exists " + MysqlEnv.tableName + ""//
			).executeUpdate();
		// if (!conn2.getAutoCommit) conn2.commit()
		MysqlDataSourcePool.retrunBack(conn2)
		logDebug("----------------after clean test table")
		logDebug("----------------test-env cleanup")
	}
}

object MysqlDataSourcePool extends HikariDataSourcePool(MysqlEnv.dbProps, MySQLDialect) { }

object MysqlDataSourceHolder extends DataSourcetHolder(MysqlDataSourcePool, TransIso.TS_SERIALIZABLE)

class MysqlTestPoolDao(dataSource: DataSourcetHolder) extends Logging {
	def conn() = dataSource.connection

	def getById(id: String): Option[User] = {
		logTrace("before query")
		if (null == id)  {
			throw new RuntimeException("User id Cannot be null")
		}
		val result: Option[User] = {
			val prep = dataSource.connection.get.prepareStatement(//
					"select * from " + MysqlEnv.tableName + " where id = ? ")
			prep.setString(1, id);
			val rs = prep.executeQuery()
			val rec = if (rs.next) {
				Some(new User(rs.getString("id"), rs.getString("name")))
			} else None
			logDebug("get user: {}", rec)
			rs.close
//			dataSource.retrunBack()    // TODO: 确认提交的逻辑
			rec
		}
		logTrace("after query")
		result
	}

	def insert(model: User): Try[Unit] = {
		logTrace("before insert")
		val res = if (null != model && null != model.id) {
			val prep = dataSource.connection.get.prepareStatement( //
					"insert into " + MysqlEnv.tableName + //
					" (id, name) values (?, ?)")

			prep.setString(1, model.id);
			prep.setString(2, model.name);
			prep.addBatch();

			prep.executeBatch()
//			dataSource.retrunBack() // TODO: 确认提交的逻辑
			Success(())
		} else Failure(new RuntimeException("Exception for Text"))
		logTrace("after insert")
		res
	}

}

@RunWith(classOf[JUnitRunner])
class MySqlDaoTest extends FunSuite with Logging {

	test("Test-session-pool-00-get-session") {
		MysqlEnv.testInEnv(() => {
			logInfo("......................... create new session\n")
			val s1 = MysqlDataSourcePool.borrow()
			assert(s1.isSuccess)
			MysqlDataSourcePool.retrunBack(s1)
		})
	}

	test("Test-session-pool-01-re-use-session") {
		MysqlEnv.testInEnv(() => {
			logInfo("......................... create new session\n")
			val s1 = MysqlDataSourcePool.borrow()
			val s2 = MysqlDataSourcePool.borrow()
			val s3 = MysqlDataSourcePool.borrow()
			assert(s1.isSuccess)
			assert(s2.isSuccess)
			assert(s3.isSuccess)
			logInfo("......................... close session\n")
			MysqlDataSourcePool.retrunBack(s1)
			MysqlDataSourcePool.retrunBack(s2)
			MysqlDataSourcePool.retrunBack(s3)
			logInfo("......................... re-use in pool\n")
			val s4 = MysqlDataSourcePool.borrow()
			val s5 = MysqlDataSourcePool.borrow()
			val s6 = MysqlDataSourcePool.borrow()
			assert(s4.isSuccess)
			assert(s5.isSuccess)
			assert(s6.isSuccess)
			logInfo("......................... close again\n")
			MysqlDataSourcePool.retrunBack(s4)
			MysqlDataSourcePool.retrunBack(s5)
			MysqlDataSourcePool.retrunBack(s6)
			logInfo("......................... all closed\n")
		})
	}

	test("Test-session-pool-02-pool-is-full") {
		MysqlEnv.testInEnv(() => {
			logDebug("......................... before create new session\n")
			val s0 = MysqlDataSourcePool.borrow()
			val s1 = MysqlDataSourcePool.borrow()
			val s2 = MysqlDataSourcePool.borrow()
			val s3 = MysqlDataSourcePool.borrow()
			val s4 = MysqlDataSourcePool.borrow()
			val s5 = MysqlDataSourcePool.borrow()
			val s6 = MysqlDataSourcePool.borrow()
			val s7 = MysqlDataSourcePool.borrow()
			val s8 = MysqlDataSourcePool.borrow()
			val s9 = MysqlDataSourcePool.borrow()
			logDebug("......................... after create new session\n")
			assert(s0.isSuccess)
			assert(s1.isSuccess)
			assert(s2.isSuccess)
			assert(s3.isSuccess)
			assert(s4.isSuccess)
			assert(s5.isSuccess)
			assert(s6.isSuccess)
			assert(s7.isSuccess)
			assert(s8.isSuccess)
			assert(s9.isSuccess)
			logDebug("......................... before pool overfool\n")
			val sa = MysqlDataSourcePool.borrow()
			logDebug("......................... after pool overfool\n")
			assert(sa.isFailure)
			logDebug("......................... clean up\n")
			MysqlDataSourcePool.retrunBack(s0)
			MysqlDataSourcePool.retrunBack(s1)
			MysqlDataSourcePool.retrunBack(s2)
			MysqlDataSourcePool.retrunBack(s3)
			MysqlDataSourcePool.retrunBack(s4)
			MysqlDataSourcePool.retrunBack(s5)
			MysqlDataSourcePool.retrunBack(s6)
			MysqlDataSourcePool.retrunBack(s7)
			MysqlDataSourcePool.retrunBack(s8)
			MysqlDataSourcePool.retrunBack(s9)
		})
	}

	test("Test-trans-00-auto-commit") {
		MysqlEnv.testInEnv(() => {
			logDebug("------------------------test auto commit\n")
			val dao = new MysqlTestPoolDao(MysqlDataSourceHolder)
			val user = new User("1", "jade")
			MysqlDataSourceHolder.connection().get.setAutoCommit(true)
			dao.insert(user)
			MysqlDataSourceHolder.retrunBack()
			logDebug("--------userid {} is {}", user.id, dao.getById(user.id).get.name)
			MysqlDataSourceHolder.retrunBack()
		})
	}
	
	test("Test-trans-01-manual-commit") {
		MysqlEnv.testInEnv(() => {
			logDebug("------------------------test manual commit\n")
			val dao = new MysqlTestPoolDao(MysqlDataSourceHolder)
			val user = new User("1", "jade")
			MysqlDataSourceHolder.connection().get.setAutoCommit(false)
			val savepoint = MysqlDataSourceHolder.connection().get.setSavepoint("" + System.currentTimeMillis())
			dao.insert(user)
			MysqlDataSourceHolder.connection().get.commit(); 
			MysqlDataSourceHolder.retrunBack()
			logDebug("--------userid {} is {}", user.id, dao.getById(user.id).get.name)
			MysqlDataSourceHolder.retrunBack()
		})
	}

	test("Test-trans-02-rollback-manual") {
		MysqlEnv.testInEnv(() => {
			logDebug("------------------------test rollback manual\n")
			val dao = new MysqlTestPoolDao(MysqlDataSourceHolder)
			MysqlDataSourceHolder.connection().get.setAutoCommit(false)
			dao.insert(new User("1", "jade"))
			dao.insert(new User("2", "yun"))
			MysqlDataSourceHolder.connection().get.commit()
			dao.insert(new User("3", "wendy"))
			dao.insert(new User("4", "wen"))
			logDebug("--------userid {} is {}", "3", dao.getById("3").get.name)
			logDebug("--------userid {} is {}", "4", dao.getById("4").get.name)
			assert("wendy"    == dao.getById("3").get.name)
			assert("wen"      == dao.getById("4").get.name)
			MysqlDataSourceHolder.connection().get.rollback()
			dao.insert(new User("5", "tiantian"))
			//
			assert("jade"     == dao.getById("1").get.name)
			assert("yun"      == dao.getById("2").get.name)
			assert(true       == dao.getById("3").isEmpty)
			assert(true       == dao.getById("4").isEmpty)
			assert("tiantian" == dao.getById("5").get.name)
			MysqlDataSourceHolder.connection().get.commit()
			MysqlDataSourceHolder.retrunBack()
		})
	}


	test("Test-trans-02-rollback-manual-savepoint") {
		MysqlEnv.testInEnv(() => {
			logInfo("------------------------test create database\n")
			val dao = new MysqlTestPoolDao(MysqlDataSourceHolder)
			MysqlDataSourceHolder.connection().get.setAutoCommit(false)
			dao.insert(new User("1", "jade"))
			dao.insert(new User("2", "yun"))
			val savepoint = MysqlDataSourceHolder.connection().get.setSavepoint("" + System.currentTimeMillis())
			dao.insert(new User("3", "wendy"))
			dao.insert(new User("4", "wen"))
			logInfo("--------userid {} is {}", "3", dao.getById("3").get.name)
			logInfo("--------userid {} is {}", "4", dao.getById("4").get.name)
			assert("wendy"     == dao.getById("3").get.name)
			assert("wen"      == dao.getById("4").get.name)
			MysqlDataSourceHolder.connection().get.rollback(savepoint)
			dao.insert(new User("5", "tiantian"))
			//
			assert("jade"     == dao.getById("1").get.name)
			assert("yun"      == dao.getById("2").get.name)
			assert(true       == dao.getById("3").isEmpty)
			assert(true       == dao.getById("4").isEmpty)
			assert("tiantian" == dao.getById("5").get.name)
			MysqlDataSourceHolder.connection().get.commit()
			MysqlDataSourceHolder.retrunBack()
		})
	}
	
	test("Test-trans-03-rollback-by-exception") {
		MysqlEnv.testInEnv(() => {
			logInfo("------------------------test rollback by exception\n")
			val dao = new MysqlTestPoolDao(MysqlDataSourceHolder)
			MysqlDataSourceHolder.connection().get.setAutoCommit(false)
			dao.insert(new User("1", "jade"))
			dao.insert(new User("2", "yun"))
			dao.insert(new User("3", "wendy"))
			dao.insert(new User("4", "wen"))
			assert("jade"  == dao.getById("1").get.name)
			assert("yun"   == dao.getById("2").get.name)
			assert("wendy" == dao.getById("3").get.name)
			assert("wen"   == dao.getById("4").get.name)
			//
			intercept[java.lang.RuntimeException] {
				try {
					val tr = dao.insert(new User(null, "tiantian"))
					if (tr.isFailure) throw tr.failed.get
				} catch {
					case e: RuntimeException => {
						MysqlDataSourceHolder.connection().get.rollback();
						throw e
					}
				}
			}
			MysqlDataSourceHolder.connection().get.commit()
			assert(dao.getById("1").isEmpty)
			assert(dao.getById("2").isEmpty)
			assert(dao.getById("3").isEmpty)
			assert(dao.getById("4").isEmpty)
			assert(dao.getById("5").isEmpty)
			MysqlDataSourceHolder.retrunBack()
		})
	}
	
	test("Test-Set") {
	  val tregex = """:([-_0-9a-zA-Z]+)""".r
	  val sql = "insert into user (id, name, age) values (:id , :name , :age)"
	  println("sql is" + tregex.replaceAllIn(sql, "?"))
	  for (m <- tregex findAllMatchIn sql) {
	    println("elems : " + m.group(1))
	  }
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