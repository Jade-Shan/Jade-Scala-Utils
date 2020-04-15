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

import org.apache.commons.lang.StringUtils.{isBlank => isBlankStr}

/* 对应不同的数据库方言 */
trait Dialect {
	def sqlTableName(database: String, table: String): String
}

class DialectDefaultImpl extends Dialect {
	
	def sqlTableName(database: String, table: String) = {
		if (isBlankStr(database)) s"`$table`" else s"`$database`.`$table`"
	}

}

object DialectDefault extends DialectDefaultImpl

object DialectSqlite extends DialectDefaultImpl {
	
	override def sqlTableName(database: String, table: String) =  s"`$table`"
}

object DialectMySQL extends DialectDefaultImpl

trait DataSourcePool {
	
	def dialect(): Dialect
	
	def borrow(): Try[Connection]

	def retrunBack(connection: Connection): Unit

	def retrunBack(connection: Try[Connection]): Unit = connection match {
		case Success(conn) => retrunBack(conn)
		case _ => { /* do nothing */}
	}
}


class HikariDataSourcePool(val props: Properties) extends DataSourcePool with Logging {

	val sqlDialectName = props.getProperty("dialect")
	val _dialect: Dialect = try {
		val d = Class.forName(sqlDialectName).getDeclaredConstructor(//
				Seq.empty[Class[_]]: _*).newInstance()
		d.asInstanceOf[Dialect]
	} catch {
		case e: Exception => DialectDefault
	}

	val cfg = new HikariConfig(props)
	val ds = new HikariDataSource(cfg)
//    val cfg = new HikariConfig()
//    cfg.setPoolName(getClass().getName())
//    cfg.setDriverClassName(driverClassName)
//    cfg.setJdbcUrl(url)
//    cfg.setUsername(username)
//    cfg.setPassword(password)
//    cfg.setMaximumPoolSize(maximumPoolSize)
//    cfg.setMaxLifetime(maxLifetime)
//    cfg.setConnectionTimeout(connectionTimeout)
//    cfg.setIdleTimeout(idleTimeout)
//    val ds = new HikariDataSource(jdbcConfig)

	def dialect(): Dialect = _dialect

	def borrow(): Try[Connection] = try Success(ds.getConnection()) catch {
		case e: Exception => Failure(e)
	}

	def retrunBack(connection: Connection): Unit = {
		if (!connection.isClosed) connection.close()
	}
	
}
