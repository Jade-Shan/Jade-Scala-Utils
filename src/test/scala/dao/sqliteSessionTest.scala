package jadeutils.comm.dao

import java.sql.DriverManager
import java.sql.Connection

import jadeutils.common.Logging
import jadeutils.common.EnvPropsComponent

import org.scalatest.junit.JUnitRunner
import org.scalatest.FunSuite
import org.junit.runner.RunWith
import java.util.Properties

object SqliteEnv {
	val dbName = "db-test-01.db"
	val tableName = "testuser"
	val dbProps = new Properties();
	dbProps.setProperty("dataSourceClassName", "org.sqlite.SQLiteDataSource");
	dbProps.setProperty("jdbcUrl", "jdbc:sqlite:" + dbName);
	dbProps.setProperty("autoCommit", "true");
	dbProps.setProperty("maximumPoolSize", "10");

	def testInEnv(opts: () => Unit) {
		val conn = SqliteDataSourcePool.borrow().get
//		val stat = conn.createStatement()
		conn.prepareStatement( //
			"drop table if exists " + SqliteEnv.tableName + "" //
		).executeUpdate()
		conn.prepareStatement( //
			"create table " + SqliteEnv.tableName + " (id, name)" //
		).executeUpdate()
		if (!conn.getAutoCommit) conn.commit()
		SqliteDataSourcePool.retrunBack(conn)
		opts()
		val conn2 = SqliteDataSourcePool.borrow().get
		conn2.prepareStatement( //
			"drop table if exists " + SqliteEnv.tableName + "" //
		).executeUpdate()
		if (!conn2.getAutoCommit) conn2.commit()
		SqliteDataSourcePool.retrunBack(conn2)
	}
}

object SqliteDataSourcePool extends HikariDataSourcePool(SqliteEnv.dbProps) { }

object SqliteDataSourceHolder extends DataSourcetHolder(SqliteDataSourcePool, TransIso.TS_SERIALIZABLE)

class User(val id: String, val name: String) {
	override def toString: String = "{%s, %s}".format(id, name)
}

class UserSqliteDao(dataSource: DataSourcetHolder) extends Dao[User, String] with Logging {
	def conn() = dataSource.connection

	def getById(id: String): Option[User] = {
		logTrace("before query")
		if (null == id)  {
			throw new RuntimeException("User id Cannot be null")
		}
		val result: Option[User] = {
			val prep = dataSource.connection.get.prepareStatement("select * from " + //
					SqliteEnv.tableName + " where id = ? ")
			prep.setString(1, id);
			val rs = prep.executeQuery()
			val rec = if (rs.next) {
				Some(new User(rs.getString("id"), rs.getString("name")))
			} else None
			logDebug("get user: {}", rec)
			rs.close
//			dataSource.retrunBack()   // TODO: 确认提交的逻辑
			rec
		}
		logTrace("after query")
		result	
	}

	def insert(model: User): Unit = {
		logTrace("before insert")
		val res = if (null != model && null != model.id) {
			val prep = dataSource.connection.get.prepareStatement( //
					"insert into " + SqliteEnv.tableName + " values (?, ?)")

			prep.setString(1, model.id);
			prep.setString(2, model.name);
			prep.addBatch();

			prep.executeBatch()
//			dataSource.retrunBack() // TODO: 确认提交的逻辑
		} else throw new RuntimeException("Exception for Text")
		logTrace("after insert")
	}

}

@RunWith(classOf[JUnitRunner])
class SqliteDaoTest extends FunSuite with Logging {

	test("Test-session-pool-00-get-session") {
		SqliteEnv.testInEnv(() => {
			logInfo("......................... create new session\n")
			val s1 = SqliteDataSourcePool.borrow()
			assert(s1.isSuccess)
			SqliteDataSourcePool.retrunBack(s1)
		})
	}

	test("Test-session-pool-01-re-use-session") {
		SqliteEnv.testInEnv(() => {
			logInfo("......................... create new session\n")
			val s1 = SqliteDataSourcePool.borrow()
			val s2 = SqliteDataSourcePool.borrow()
			val s3 = SqliteDataSourcePool.borrow()
			assert(s1.isSuccess)
			assert(s2.isSuccess)
			assert(s3.isSuccess)
			logInfo("......................... close session\n")
			SqliteDataSourcePool.retrunBack(s1)
			SqliteDataSourcePool.retrunBack(s2)
			SqliteDataSourcePool.retrunBack(s3)
			logInfo("......................... re-use in pool\n")
			val s4 = SqliteDataSourcePool.borrow()
			val s5 = SqliteDataSourcePool.borrow()
			val s6 = SqliteDataSourcePool.borrow()
			assert(s4.isSuccess)
			assert(s5.isSuccess)
			assert(s6.isSuccess)
			logInfo("......................... close again\n")
			SqliteDataSourcePool.retrunBack(s4)
			SqliteDataSourcePool.retrunBack(s5)
			SqliteDataSourcePool.retrunBack(s6)
		})
	}

	test("Test-session-pool-02-pool-is-full") {
		SqliteEnv.testInEnv(() => {
			logInfo("......................... create new session\n")
			val s0 = SqliteDataSourcePool.borrow()
			val s1 = SqliteDataSourcePool.borrow()
			val s2 = SqliteDataSourcePool.borrow()
			val s3 = SqliteDataSourcePool.borrow()
			val s4 = SqliteDataSourcePool.borrow()
			val s5 = SqliteDataSourcePool.borrow()
			val s6 = SqliteDataSourcePool.borrow()
			val s7 = SqliteDataSourcePool.borrow()
			val s8 = SqliteDataSourcePool.borrow()
			val s9 = SqliteDataSourcePool.borrow()
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
			// 最大10个连接，外层的的`testInEnv()`建了一个，
			// 加上这里建立的9个，已经满了
			logInfo("......................... pool overfool\n")
			val sa = SqliteDataSourcePool.borrow()
			assert(sa.isFailure)
			logInfo("......................... clean up\n")
			SqliteDataSourcePool.retrunBack(s1)
			SqliteDataSourcePool.retrunBack(s2)
			SqliteDataSourcePool.retrunBack(s3)
			SqliteDataSourcePool.retrunBack(s4)
			SqliteDataSourcePool.retrunBack(s5)
			SqliteDataSourcePool.retrunBack(s6)
			SqliteDataSourcePool.retrunBack(s7)
			SqliteDataSourcePool.retrunBack(s8)
			SqliteDataSourcePool.retrunBack(s9)
		})
	}

	test("Test-trans-00-auto-commit") {
		SqliteEnv.testInEnv(() => {
			logInfo("------------------------test auto commit\n")
			val dao = new UserSqliteDao(SqliteDataSourceHolder)
			val user = new User("1", "jade")
			SqliteDataSourceHolder.connection.get.setAutoCommit(true)
			dao.insert(user)
			SqliteDataSourceHolder.retrunBack()
			logInfo("--------userid {} is {}", user.id, dao.getById(user.id).get)
			SqliteDataSourceHolder.retrunBack()
		})
	}

	test("Test-trans-01-manual-commit") {
		SqliteEnv.testInEnv(() => {
			logInfo("------------------------test manual commit\n")
			val dao = new UserSqliteDao(SqliteDataSourceHolder)
			val user = new User("1", "jade")
			SqliteDataSourceHolder.connection.get.setAutoCommit(false)
			dao.insert(user)
			SqliteDataSourceHolder.connection.get.commit();
			SqliteDataSourceHolder.retrunBack()
			logInfo("--------userid {} is {}", user.id, dao.getById(user.id).get)
			SqliteDataSourceHolder.retrunBack()
		})
	}
	
	
	test("Test-trans-02-rollback-manual") {
		SqliteEnv.testInEnv(() => {
			logInfo("------------------------test rollback manual\n")
			val dao = new UserSqliteDao(SqliteDataSourceHolder)
			SqliteDataSourceHolder.connection.get.setAutoCommit(false)
			dao.insert(new User("1", "jade"))
			dao.insert(new User("2", "yun"))
			dao.insert(new User("3", "wendy"))
			dao.insert(new User("4", "wen"))
			dao.insert(new User("5", "tiantian"))
			assert("jade"     == dao.getById("1").get.name)
			assert("yun"      == dao.getById("2").get.name)
			assert("wendy"    == dao.getById("3").get.name)
			assert("wen"       == dao.getById("4").get.name)
			assert("tiantian" == dao.getById("5").get.name)
			SqliteDataSourceHolder.connection.get.rollback()
			assert(dao.getById("1").isEmpty)
			assert(dao.getById("2").isEmpty)
			assert(dao.getById("3").isEmpty)
			assert(dao.getById("4").isEmpty)
			assert(dao.getById("5").isEmpty)
			SqliteDataSourceHolder.retrunBack()
		})
	}
	
	test("Test-trans-03-rollback-by-exception") {
		SqliteEnv.testInEnv(() => {
			logInfo("------------------------test rollback by exception\n")
			val dao = new UserSqliteDao(SqliteDataSourceHolder)
			SqliteDataSourceHolder.connection.get.setAutoCommit(false)
			dao.insert(new User("1", "jade"))
			dao.insert(new User("2", "yun"))
			dao.insert(new User("3", "wendy"))
			dao.insert(new User("4", "wen"))
			assert("jade"  == dao.getById("1").get.name)
			assert("yun"   == dao.getById("2").get.name)
			assert("wendy" == dao.getById("3").get.name)
			assert("wen"    == dao.getById("4").get.name)
			intercept[java.lang.RuntimeException] {
				try {
					dao.insert(new User(null, "tiantian"))
				} catch { case e: RuntimeException => 
					if (!SqliteDataSourceHolder.connection.get.getAutoCommit) {
						SqliteDataSourceHolder.connection.get.rollback();
						throw e
					}
				}
			}
			if (!SqliteDataSourceHolder.connection.get.getAutoCommit) { 
				SqliteDataSourceHolder.connection.get.commit() 
			}
			assert(dao.getById("1").isEmpty)
			assert(dao.getById("2").isEmpty)
			assert(dao.getById("3").isEmpty)
			assert(dao.getById("4").isEmpty)
			assert(dao.getById("5").isEmpty)
			SqliteDataSourceHolder.retrunBack()
		})
	}

}

//	class TestBaseService extends BaseTransactionService {
//		val daoSessPool = SqliteDaoSessionPool
//	}

//
//	test("Test-trans-02") {
//		testInEnv((conn) => {
//			logInfo("......................... will commit\n")
//
//			object UserService extends TestBaseService {
//				private val dao = new UserDao(SqliteDaoSessionPool)
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
//				private val dao = new UserDao(SqliteDaoSessionPool)
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
