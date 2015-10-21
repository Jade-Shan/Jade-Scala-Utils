package jadeutils.comm.dao

import jadeutils.common.Logging
import jadeutils.common.EnvPropsComponent

import org.scalatest.junit.JUnitRunner
import org.scalatest.FunSuite
import org.junit.runner.RunWith

@RunWith(classOf[JUnitRunner])
class SqliteDaoTest extends FunSuite with Logging {

	class TestTransaction(val id: String) extends Transaction with Logging {
		def getId = id
		private[this] var transStatus = "ready"

		def isActive = "active" == transStatus
		def begin() { transStatus = "active" }
		def commit() { transStatus = "commited" }
		def rollback() { transStatus = "rollbacked" }
	}

	class BaseDaoSession(id: String, dbConnection: java.sql.Connection, 
		factory: BaseDaoSessionFactory) extends DaoSession with Logging 
	{
		private[this] val conn = dbConnection
		private[this] var transCount = 0
		private[this] var autoCommit = true
		private[this] var sessId = "BasicDaoSession: " + id

		logTrace("DaoSession create: {}", sessId)

		def getId = sessId
		def isBroken = conn.isClosed
		def isAutoCommit = autoCommit
		def getTransaction() = if (null != trans) trans else {
			trans = new TestTransaction("" + transCount)
			transCount = transCount + 1
			trans
		}

		def close() {
			factory.close(this)
			logTrace("DaoSession close: {}", sessId)
		}

		def setAutoCommit(isAuto: Boolean) { autoCommit = isAuto }
	}

	abstract class BaseDaoSessionFactory extends DaoSessionFactory with Logging { 
		private[this] val conn = new ThreadLocal[DaoSession]; 

		val initPoolSize = 5
		val minPoolSize = 3
		val maxPoolSize = 10

		private[this] val idleSess = {
			new scala.collection.mutable.Stack[DaoSession]
		}

		private[this] val actSesss = {
			new scala.collection.mutable.HashMap[String, DaoSession]
		}

		private[this] def size() = idleSess.size + actSesss.size


		def createSession(): DaoSession = {
			logTrace("size: {} >= max: {}", size, maxPoolSize)
			if (size >= maxPoolSize) 
				throw new RuntimeException("Db connection Pool filled")

			val sess = nextSession()
			actSesss.put(sess.getId, sess)

			logTrace("size: {} ----- max: {} idle: {} \n active: {}", 
				size, maxPoolSize, idleSess, actSesss)
			sess
		}

		private[this] def nextSession(): DaoSession = {
			val session  = if (idleSess.size < 1) {
				new BaseDaoSession("" + size, createConnection(), this)
			} else idleSess.pop

			if (session.isBroken) {
				nextSession()  // drop borken session, find next idle session
			} else session
		}

		def close(session: DaoSession) {
			if (actSesss.contains(session.getId)) {
				actSesss.remove(session.getId)
				idleSess.push(session)
			}
			logTrace("size: {} ----- max: {} idle: {} \n active: {}", 
				size, maxPoolSize, idleSess, actSesss)
		}

	}

	object SqliteDaoSessionFactory extends BaseDaoSessionFactory {
		def createConnection = java.sql.DriverManager.getConnection(
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
		private val dao = new UserDao(getSession)

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
		import java.sql._

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
