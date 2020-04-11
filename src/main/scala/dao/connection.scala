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
import scala.util.Failure

trait DataSourcePool {

	def borrow(): Try[Connection]

	def retrunBack(connection: Connection): Unit

	def retrunBack(connection: Try[Connection]): Unit = connection match {
		case Success(conn) => retrunBack(conn)
		case _ => { /* do nothing */}
	}
}


class HikariDataSourcePool(val props: Properties) extends DataSourcePool with Logging {

	val cfg = new HikariConfig(props);
	val ds = new HikariDataSource(cfg);
	  val cfg2 = new HikariConfig();  
//    cfg.setPoolName(getClass().getName());  
//    cfg.setDriverClassName(driverClassName);  
//    cfg.setJdbcUrl(url);  
//    cfg.setUsername(username);  
//    cfg.setPassword(password);  
//    cfg.setMaximumPoolSize(maximumPoolSize);  
//    cfg.setMaxLifetime(maxLifetime);  
//    cfg.setConnectionTimeout(connectionTimeout);  
//    cfg.setIdleTimeout(idleTimeout);  
//    val ds = new HikariDataSource(jdbcConfig)
	def borrow(): Try[Connection] = try Success(ds.getConnection()) catch {
		case e: Exception => Failure(e)
	}

	def retrunBack(connection: Connection): Unit = {
		if (!connection.isClosed) connection.close()
	}
		
	
}

