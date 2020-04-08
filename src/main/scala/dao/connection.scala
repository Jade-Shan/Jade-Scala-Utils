package jadeutils.comm.dao

import java.util.Properties

import com.zaxxer.hikari.HikariConfig;
import com.zaxxer.hikari.HikariDataSource;
import com.zaxxer.hikari.util.DriverDataSource;

import jadeutils.common.Logging
import java.sql.Connection
import scala.util.Try
import scala.util.Success
import java.sql.SQLException
import org.scalactic.Fail
import scala.util.Failure

trait DatabaseConnectionPool {

	def borrow(): Try[Connection]
}


class HikariConnectionPool(val props: Properties) extends DatabaseConnectionPool with Logging {

	val cfg = new HikariConfig(props);
	val ds = new HikariDataSource(cfg);

	def borrow(): Try[Connection] = try {
		Success(ds.getConnection())
	} catch {
		case e: SQLException => Failure(e)
	}
	
	def retrunBack(connection: Try[Connection]): Unit = connection match {
		case Success(conn) => conn.close()
		case Failure(e) => { /* do nothing */}
	}
		
	
}

