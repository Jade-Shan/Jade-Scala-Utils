package jadeutils.comm.dao


import java.sql.DriverManager
import java.sql.Connection

import jadeutils.common.Logging
import jadeutils.common.EnvPropsComponent

import org.scalatest.junit.JUnitRunner
import org.scalatest.FunSuite
import org.junit.runner.RunWith



@RunWith(classOf[JUnitRunner])
class BaseTransWrapperTest extends FunSuite with Logging {
	import jadeutils.comm.dao.TransIso

	val dbName = "db-test-01"
	val tableName = "testuser"


}