package jadeutils.comm.dao

import java.sql.DriverManager
import java.sql.Connection

import jadeutils.common.Logging
import jadeutils.common.EnvPropsComponent

import org.scalatest.junit.JUnitRunner
import org.scalatest.FunSuite
import org.junit.runner.RunWith

@RunWith(classOf[JUnitRunner])
class SqliteDaoTest extends FunSuite with Logging {
	import jadeutils.comm.dao.TransIso

	val dbName = "db-test-01.db"
	val tableName = "testuser"

	object SqliteDaoSessionFactory extends DaoSessionFactory (3, 10, 5) {
		val defaultIsolation = TransIso.TS_SERIALIZABLE

		def connectDB(): java.sql.Connection = {
			Class.forName("org.sqlite.JDBC")
			DriverManager.getConnection(
				"jdbc:sqlite:" + dbName)
		}
	}

	class TestBaseService extends BaseTransactionService {
		val sessionFactory = SqliteDaoSessionFactory
	}

	class User(val id: String, val name: String) {
		override def toString: String = "{%s, %s}".format(id, name)
	}

	class UserDao(sessionFactory: DaoSessionFactory) 
	extends Dao[User, String] with Logging 
	{
		def session() = sessionFactory.currentSession
		def conn() = session.conn

		def getById(id: String): User = {
			logTrace("before query")
			val u = if (null != id) {
				val prep = conn.prepareStatement("select * from " + tableName + " where id = ?;")
				prep.setString(1, id);
				val rs = prep.executeQuery()
				val rec = if (rs.next) {
					new User(rs.getString("id"), rs.getString("name"))
				} else null
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

	object UserService extends TestBaseService {
		private val dao = new UserDao(SqliteDaoSessionFactory)

		def getUser(id: String): User = withTransaction { dao.getById(id) }
		def insertUser(user: User) { withTransaction { dao.insert(user) } }

		def insertUserList(userlist: List[User]) {withTransaction { 
			userlist.foreach((user) => {dao.insert(user)})
		}}
	}

	def testInEnv(opts: (Connection) => Unit) {
		Class.forName("org.sqlite.JDBC")
		val conn = DriverManager.getConnection("jdbc:sqlite:" + dbName)
		val stat = conn.createStatement()
		stat.executeUpdate("drop table if exists " + tableName + ";")
		stat.executeUpdate("create table " + tableName + " (id, name);")
		opts(conn)
		stat.executeUpdate("drop table if exists " + tableName + ";")
		conn.close();
	}

	test("Test-Trans-commit") {
		testInEnv((conn) => {
				logInfo("......................... will commit\n")
				UserService.insertUserList(new User("1", "jade") :: 
					new User("2", "yun") :: new User("3", "wendy") :: 
					new User("4", "wen") :: new User("5", "tiantian") :: Nil)
				val u1 = UserService.getUser("1")
				val u2 = UserService.getUser("2")
				val u3 = UserService.getUser("3")
				val u4 = UserService.getUser("4")
				val u5 = UserService.getUser("5")
				assert("1" == u1.id && "jade" == u1.name)
				assert("2" == u2.id && "yun" == u2.name)
				assert("3" == u3.id && "wendy" == u3.name)
				assert("4" == u4.id && "wen" == u4.name)
				assert("5" == u5.id && "tiantian" == u5.name)
			})
	}

	test("Test-trans-rollback") {
		testInEnv((conn) => {
				logInfo("......................... will rollback\n")
				intercept[java.lang.Exception] {
					UserService.insertUserList(new User("1", "jade") :: 
						new User("2", "yun") :: new User("3", "wendy") :: 
						new User("4", "wen") :: new User(null, "tiantian") :: Nil)
				}

				assert(null == UserService.getUser("1"))
				assert(null == UserService.getUser("2"))
				assert(null == UserService.getUser("3"))
				assert(null == UserService.getUser("4"))
				assert(null == UserService.getUser("5"))
			})
	}

}
