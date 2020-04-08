package jadeutils.comm.dao

import java.util.Properties

import com.zaxxer.hikari.HikariConfig;
import com.zaxxer.hikari.HikariDataSource;
import com.zaxxer.hikari.util.DriverDataSource;

import jadeutils.common.Logging
import java.sql.Connection

trait DatabaseConnectionPool {

	def getConnection(): Connection
}


class HikariConnectionPool(val props: Properties) extends DatabaseConnectionPool with Logging {

	val cfg = new HikariConfig(props);
	val ds = new HikariDataSource(cfg);

	def getConnection() = ds.getConnection()
}

