package jadeutils.comm.dao

import java.sql.DriverManager
import java.sql.Connection

import jadeutils.common.Logging
import jadeutils.common.EnvPropsComponent

import org.scalatest.junit.JUnitRunner
import org.scalatest.FunSuite
import org.junit.runner.RunWith

object SqliteEnv {
	val dbName = "db-test-01.db"
	val tableName = "testuser"

	def testInEnv(opts: (Connection) => Unit) {
		val conn = SqliteDaoSessionPool.current.right.get.conn
		val stat = conn.createStatement()
		conn.prepareStatement( //
			"drop table if exists " + SqliteEnv.tableName + "" //
		).executeUpdate();
		conn.prepareStatement( //
			"create table " + SqliteEnv.tableName + " (id, name)" //
		).executeUpdate();
		opts(conn)
		conn.prepareStatement( //
			"drop table if exists " + SqliteEnv.tableName + "" //
		).executeUpdate();
		conn.close();
	}
}

object SqliteDaoSessionPool extends DaoSessionPool(3, 10, 5) {
	val defaultIsolation = TransIso.TS_SERIALIZABLE

	def connectDB() = {
		Class.forName("org.sqlite.JDBC")
		Right(DriverManager.getConnection(
			"jdbc:sqlite:" + SqliteEnv.dbName
		))
	}
}

class User(val id: String, val name: String) {
	override def toString: String = "{%s, %s}".format(id, name)
}

class UserSqliteDao(pool: DaoSessionPool) extends Dao[User, String] with Logging {
	def session() = pool.current
	def conn() = session.right.get.conn

	def getById(id: String): Option[User] = {
		logTrace("before query")
		if (null == id)  {
			throw new RuntimeException("User id Cannot be null")
		}
		val result: Option[User] = {
			val prep = conn.prepareStatement("select * from " + //
					SqliteEnv.tableName + " where id = ? ")
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
		result	
	}

	def insert(model: User): Unit = {
		logTrace("before insert")
		val res = if (null != model && null != model.id) {
			val prep = conn.prepareStatement( //
					"insert into " + SqliteEnv.tableName + " values (?, ?)")

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
class SqliteDaoTest extends FunSuite with Logging {

	test("Test-trans-00-auto-commit") {
		SqliteEnv.testInEnv((conn) => {
			logInfo("------------------------test auto commit\n")
			val dao = new UserSqliteDao(SqliteDaoSessionPool)
			val user = new User("1", "jade")
			conn.setAutoCommit(true)
			dao.insert(user)
			logInfo("--------userid {} is {}", user.id, dao.getById(user.id).get)
		})
	}

	test("Test-trans-01-manual-commit") {
		SqliteEnv.testInEnv((conn) => {
			logInfo("------------------------test manual commit\n")
			val dao = new UserSqliteDao(SqliteDaoSessionPool)
			val user = new User("1", "jade")
			conn.setAutoCommit(false)
			dao.insert(user)
			conn.commit();
			logInfo("--------userid {} is {}", user.id, dao.getById(user.id).get)
		})
	}
	
	
	test("Test-trans-02-rollback-manual") {
		SqliteEnv.testInEnv((conn) => {
			logInfo("------------------------test rollback manual\n")
			val dao = new UserSqliteDao(SqliteDaoSessionPool)
			conn.setAutoCommit(false)
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
			conn.rollback()
			assert(dao.getById("1").isEmpty)
			assert(dao.getById("2").isEmpty)
			assert(dao.getById("3").isEmpty)
			assert(dao.getById("4").isEmpty)
			assert(dao.getById("5").isEmpty)
		})
	}
	
	test("Test-trans-03-rollback-by-exception") {
		SqliteEnv.testInEnv((conn) => {
			logInfo("------------------------test rollback by exception\n")
			val dao = new UserSqliteDao(SqliteDaoSessionPool)
			conn.setAutoCommit(false)
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
				} catch {
					case e: RuntimeException => if (!conn.getAutoCommit) {
						conn.rollback();
						throw e
					}
				}
			}
			if (!conn.getAutoCommit) { conn.commit() }
			assert(dao.getById("1").isEmpty)
			assert(dao.getById("2").isEmpty)
			assert(dao.getById("3").isEmpty)
			assert(dao.getById("4").isEmpty)
			assert(dao.getById("5").isEmpty)
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
