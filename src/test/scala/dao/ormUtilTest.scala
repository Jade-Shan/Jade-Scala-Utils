package jadeutils.comm.dao

import jadeutils.common.Logging


import org.scalatest.junit.JUnitRunner
import org.scalatest.FunSuite
import org.junit.runner.RunWith

import jadeutils.database.orm.Record
import jadeutils.database.orm.Table
import jadeutils.database.orm.Column

@Table(database="db-test-01.db", table = "testuser")
class User(val id: String, val name: String) extends Record[String] {
	override def toString: String = "{%s, %s}".format(id, name)
}

@RunWith(classOf[JUnitRunner])
class ORMUtilTest extends FunSuite with Logging {

	test("Test-Table-Annotation") {
		logDebug("------------------------")
		val user = new User("1", "Jade")
		println(user)
		val map = ORMUtil.obj2kv[User, String](classOf[User], user)
		println(map)
		logDebug("------------------------obj 2 map : {}", map)
	}

}

