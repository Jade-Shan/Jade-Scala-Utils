package jadeutils.comm.dao

import java.sql.DriverManager

import jadeutils.common.Logging
import jadeutils.common.EnvPropsComponent

import org.scalatest.junit.JUnitRunner
import org.scalatest.FunSuite
import org.junit.runner.RunWith

@RunWith(classOf[JUnitRunner])
class SqliteDaoTest extends FunSuite with Logging {
	object SqliteDaoSessionFactory extends DaoSessionFactory {
		def createConnection() = DriverManager.getConnection(
			"jdbc:sqlite:test.db")
	}

	class TestBaseService extends BaseTransactionService {
		val sessionFactory = SqliteDaoSessionFactory
	}

	class User(val id: Int, val name: String) {
		override def toString: String = "{%d, %s}".format(id, name)
	}

	class UserDao(session: DaoSession) extends Dao[User, Int] with Logging {

		def getById(id: Int): User = {
			logTrace("before query")

			val u = if (id > 0) new User(id, "TestUser" + id)
			else throw new RuntimeException("Exception for Text")

			logTrace("after query")
			u
		}

		def insert(model: User)  {
			logTrace("before insert")
			if (null == model) 
				throw new RuntimeException("Exception for Text")
			logTrace("after insert")
		}

	}

	object UserService extends TestBaseService {
		private val dao = new UserDao(sessionFactory.currentSession)

		def getUser(id: Int): User = withTransaction { dao.getById(id) }
		def insertUser(user: User) { withTransaction { dao.insert(user) } }
	}

	test("Test-session-pool") {
		val fc = SqliteDaoSessionFactory
		logInfo("......................... create new session\n")
		val c1 = fc.createSession()
		val c2 = fc.createSession()
		val c3 = fc.createSession()
		val c4 = fc.createSession()
		val c5 = fc.createSession()
		val c6 = fc.createSession()
		logInfo("......................... close session\n")
		c1.close
		c2.close
		c3.close
		logInfo("......................... re-use in pool\n")
		val c7 = fc.createSession()
		val c8 = fc.createSession()
		val c9 = fc.createSession()
		logInfo("......................... full pool\n")
		val ca = fc.createSession()
		val cb = fc.createSession()
		val cc = fc.createSession()
		val cd = fc.createSession()
		logInfo("......................... pool overfool\n")
		intercept[java.lang.Exception] {
			val ce = fc.createSession()
		}
		logInfo("......................... clean up\n")
		c4.close
		c5.close
		c6.close
		c7.close
		c8.close
		c9.close
		ca.close
		cb.close
		cc.close
		cd.close
	}


	test("Test-Sqlite") {

		Class.forName("org.sqlite.JDBC")

		val conn = DriverManager.getConnection("jdbc:sqlite:test.db")
		val stat = conn.createStatement()
    stat.executeUpdate("drop table if exists people;")
    stat.executeUpdate("create table people (name, occupation);")
    val prep = conn.prepareStatement("insert into people values (?, ?);")

    prep.setString(1, "Gandhi");
    prep.setString(2, "politics");
    prep.addBatch();
    prep.setString(1, "Turing");
    prep.setString(2, "computers");
    prep.addBatch();
    prep.setString(1, "Wittgenstein");
    prep.setString(2, "smartypants");
    prep.addBatch();

		conn.setAutoCommit(false)
		prep.executeBatch()
		conn.setAutoCommit(true)
		val rs = stat.executeQuery("select * from people;")
		while (rs.next())
		{
			logInfo("name = " + rs.getString("name"))
			logInfo("job = " + rs.getString("occupation"))
		}
		rs.close();
		conn.close();
	}

}
