package jadeutils.comm.dao

import java.util.Properties

import com.zaxxer.hikari.HikariConfig;
import com.zaxxer.hikari.HikariDataSource;
import com.zaxxer.hikari.util.DriverDataSource;

import jadeutils.common.Logging

class ConnectionPool(val props: Properties) extends Logging 
{
	val cfg = new HikariConfig(props);
	val ds = new HikariDataSource(cfg);

	def getConnection() = ds.getConnection()
}

