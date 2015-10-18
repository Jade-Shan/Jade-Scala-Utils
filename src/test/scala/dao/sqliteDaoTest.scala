package jadeutils.comm.dao

import jadeutils.common._

import org.scalatest.junit.JUnitRunner
import org.scalatest.FunSuite
import org.junit.runner.RunWith

@RunWith(classOf[JUnitRunner])
class SqliteDaoTest extends FunSuite with Logging {


	test("Test-Sqlite") {
		// logInfo("======== test get rollback =============")
		// intercept[java.lang.Exception] {
		// 	val u = UserService.getUser(-33)
		// 	logInfo("{}", u)
		// }

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
