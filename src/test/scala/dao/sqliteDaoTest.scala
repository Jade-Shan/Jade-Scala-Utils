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
		def isActive = "active" == status

		def begin() { status = "active" }
		def commit() { status = "commited" }
		def rollback() { status = "rollbacked" }
	}

	class AbstractSession(id: String, factory: AbstractSessionFactory) 
	extends DaoSession with Logging 
	{
		private[this] var transCount = 0
		private[this] var autoCommit = true
		private[this] var sessId = "BasicDaoSession: " + id

		logTrace("DaoSession create: {}", sessId)

		def getId = sessId
		def isOpen = "open" == status
		def isAutoCommit = autoCommit
		def getTransaction() = {
			if (null != trans) trans else {
				trans = new TestTransaction("" + transCount)
				transCount = transCount + 1
				trans
			}
		}

		def open()  {
			status = "open"
			logTrace("Session {} open", id)
		}

		def close() {
			status = "closed"
			factory.close(this)
			logTrace("DaoSession close: {}", sessId)
		}

		def setAutoCommit(isAuto: Boolean) { autoCommit = isAuto }
	}

	class AbstractSessionFactory extends DaoSessionFactory with Logging { 
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
			if (size >= maxPoolSize) 
				throw new RuntimeException("Db connection Pool filled")

			val sess = if (idleSess.size < 1) {
				new AbstractSession("" + size, this)
			} else idleSess.pop

			actSesss.put(sess.getId, sess)

			sess
		}

		def close(session: DaoSession) {
			if (actSesss.contains(session.getId)) {
				actSesss.remove(session.getId)
				idleSess.push(session)
			}
		}

	}

	object TestSessionFactoryHelper extends DaoSessionFactoryHelper {

		def initSessionFactory = new AbstractSessionFactory 

	}

	class TestBaseService extends BaseTransactionService {
		val sfHelper = TestSessionFactoryHelper
	}

	class User(val id: Int, val name: String) {
		override def toString: String = "{%d, %s}".format(id, name)
	}

	class UserDao(session: DaoSession) extends BasicDao[User, Int](session) 
		with Logging
	{

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
		val fc = new AbstractSessionFactory()
		logInfo(".......... create new session")
		val c1 = fc.createSession()
		val c2 = fc.createSession()
		val c3 = fc.createSession()
		val c4 = fc.createSession()
		val c5 = fc.createSession()
		val c6 = fc.createSession()
		logInfo(".......... close session")
		c1.close
		c2.close
		c3.close
		logInfo(".......... reuse in pool")
		val c7 = fc.createSession()
		val c8 = fc.createSession()
		val c9 = fc.createSession()
		logInfo(".......... full pool")
		val ca = fc.createSession()
		val cb = fc.createSession()
		val cc = fc.createSession()
		val cd = fc.createSession()
		logInfo(".......... out of max")
		intercept[java.lang.Exception] {
			val ce = fc.createSession()
		}
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
			println("name = " + rs.getString("name"))
			println("job = " + rs.getString("occupation"))
		}
		rs.close();
		conn.close();
	}

}
