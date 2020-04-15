package jadeutils.comm.dao

import org.scalatest.junit.JUnitRunner

import org.scalatest.FunSuite
import org.junit.runner.RunWith

import jadeutils.common.Logging

import java.sql.SQLException
import java.util.Date

import jadeutils.comm.dao.DialectSqlite.{dialect => SqliteDialect}

@RunWith(classOf[JUnitRunner])
class ORMUtilTest extends FunSuite with Logging {

	val userTableColumns = Set("last_change_time", "create_time", "id", "name")

	test("Test-00-DBTable-Annotation") {
		logDebug("------------------------")
		val tablename = ORMUtil.getTableName[User, String](classOf[User], SqliteDialect)
		assert(tablename.isSuccess)
		logDebug("------------------------" + tablename.get)
//		assert("`db-test-01`.`testuser`" == tablename.get)
		assert("`testuser`" == tablename.get)
		logDebug("------------------------obj 2 map : {}", tablename)
	}

	test("Test-01-DBColumn-Annotation") {
		val user = new User("1", "Jade")
		val clazz = classOf[User]
		for (f <- clazz.getDeclaredFields()) {
			val clm: Column = f.getAnnotation(classOf[Column])
			logDebug("field ann is: {}", clm)
		}
		//
		val cols = ORMUtil.getColumns[User, String](classOf[User], Set.empty[String])
		println(cols)
		// 执行结果不应该含有标准答案中没有的元素
		for (c <- cols) assert(userTableColumns.contains(c))
		// 执行结果应该含有标准答案中所有的元素
		for (c <- userTableColumns) assert(cols.contains(c))
	}

	test("Test-02-obj-to-keyValue") {
		val now = new Date(System.currentTimeMillis())
		val user = new User("1", "Jade", now, now)
		val clazz = classOf[User]
		val seq = ORMUtil.obj2kv[User, String](clazz, user, Set.empty[String])
		var map: Map[String, Any] = Map.empty
		for (e <- seq) { map = map + e }
		// 执行结果不应该含有标准答案中没有的元素
		for (c <- map.keySet) assert(userTableColumns.contains(c))
		// 执行结果应该含有标准答案中所有的元素
		for (c <- userTableColumns) assert(map.keySet.contains(c))
		// 所有元素中的值也要和标准答案一样
		assert("1" == map.get("id").get)
		assert("Jade" == map.get("name").get)
		assert(now == map.get("create_time").get)
		assert(now == map.get("last_change_time").get)
	}

	test("Test-03-reflect-obj") {
		val cons = classOf[User].getDeclaredConstructors
		println(cons.size)
		for (con <- cons) println("ccccc " + con.getParameterTypes)
		for (con <- cons) println("dddddd " + con.getParameters)
		val con = classOf[User].getDeclaredConstructor(Seq.empty[Class[_]]: _*)
		println(con)
		println(con.newInstance())
	}

	test("Test-04-resultset-to-obj") {
		val now = new Date(System.currentTimeMillis())
		val user = new User("1", "Jade", now, now)
		SqliteEnv.testInEnv(() => {
			val now = new java.sql.Date(System.currentTimeMillis())
			val prep1 = SqliteDataSourceHolder.connection.get.prepareStatement( //
					"insert into `" + SqliteEnv.tableName + //
					"` (`id`,`name`,`last_change_time`,`create_time`) " + //
					"values (?,?,?,?)"
			)
			prep1.setString(1, user.id)
			prep1.setString(2, user.name)
			prep1.setDate(3, new java.sql.Date(user.createTime.getTime))
			prep1.setDate(4, new java.sql.Date(user.lastChangeTime.getTime))
			prep1.addBatch();
			prep1.executeBatch()
			SqliteDataSourceHolder.retrunBack()
			//
			val showCols = Set.empty[String]
			val entryClass = classOf[User]
			val table = ORMUtil.getTableName[User, String](entryClass, SqliteDialect).get
			val columns = ORMUtil.getColumns[User, String](entryClass, showCols)
			val colStr = { for (s <- columns) yield "`%s`".format(s) }.mkString(",")
			val sql = s"select $colStr from $table where id = ?"			
			logDebug(sql)
			val prep2 = SqliteDataSourceHolder.connection.get.prepareStatement( //
				"select * from " + SqliteEnv.tableName + " where id = ? "
			)
			prep2.setString(1, "1");
			val rs = prep2.executeQuery()
			val obj = ORMUtil.row2obj[User, String](classOf[User], null, rs)
			SqliteDataSourceHolder.retrunBack()
			logDebug(obj.toString)
			assert("1" == obj.id)
			assert("Jade" == obj.name)
//			assert(now.getTime == obj.createTime.getTime)
//			assert(now.getTime == obj.lastChangeTime.getTime)
		})
	}

	test("Test-05-obj-to-insert") {
		SqliteEnv.testInEnv(() => {
			val now = new Date(System.currentTimeMillis())
			val user = new User("1", "Jade", now, now)
			val table = ORMUtil.getTableName[User, String](classOf[User], SqliteDialect).get
			val seq = ORMUtil.obj2kv[User, String](classOf[User], user, Set.empty[String])
			logDebug(seq.toString)
			val xx = seq.unzip
			val colStr = { for (s <- xx._1) yield "`%s`".format(s) }.mkString(",")
			val values = xx._2
			val markStr = { for (s <- xx._1) yield "?" }.mkString(",")
			val sql = s"insert into $table ($colStr) values ($markStr)"
			logDebug(sql)
			val prep1 = SqliteDataSourceHolder.connection.get.prepareStatement(sql)
			ORMUtil.setQueryValues(prep1, values)
			prep1.addBatch()
			prep1.executeBatch()
			//
			val prep2 = SqliteDataSourceHolder.connection.get.prepareStatement( //
				"select * from " + SqliteEnv.tableName + " where id = ? "
			)
			prep2.setString(1, "1");
			val rs = prep2.executeQuery()
			val rec = ORMUtil.row2obj[User, String](classOf[User], null, rs)
			SqliteDataSourceHolder.retrunBack()
			println(rec)
		})
	}

}

// TODO: https://www.programcreek.com/scala/java.lang.reflect.Constructor
// TODO: https://www.iteye.com/blog/zk-chs-2290572
// TODO: https://blog.csdn.net/u011152627/article/details/77745132
import scala.annotation.StaticAnnotation
import scala.reflect.runtime.universe._
import scala.collection.Seq

class CstmAnt(name: String, num: Int) extends StaticAnnotation {
	override def toString = s"Annotation args: name -> $name, num -> $num"
}

@CstmAnt("Annotation for Class", 2333)
class Test {
	@CstmAnt("Annotation for Member", 6666)
	val ff = ""
	def mm(ss: String, @CstmAnt("Annotation for Arg", 9999) arg: Int) = ""
}

@RunWith(classOf[JUnitRunner])
class ScalaAnnotationTest extends FunSuite with Logging {

	test("Test-Annotation") {

		// 获取指定类型的注解信息，通过 Annotation.tree.tpe 获取注解的 Type 类型，以此进行筛选
		def getClassAnnotation[T: TypeTag, U: TypeTag] =
			symbolOf[T].annotations.find(_.tree.tpe =:= typeOf[U])

		// 通过字段名称获取指定类型的注解信息，注意查找字段名称时添加空格
		def getMemberAnnotation[T: TypeTag, U: TypeTag](memberName: String) =
			typeOf[T].decl(TermName(s"$memberName ")).annotations.find(_.tree.tpe =:= typeOf[U])

		// 通过方法名称和参数名称获取指定类型的注解信息
		def getArgAnnotation[T: TypeTag, U: TypeTag](methodName: String, argName: String) =
			typeOf[T].decl(TermName(methodName)).asMethod.paramLists.collect {
				case symbols => symbols.find(_.name == TermName(argName))
			}.headOption.fold(Option[Annotation](null))(_.get.annotations.find(_.tree.tpe =:= typeOf[U]))

		// 解析语法树，获取注解数据
		def getCustomAnnotationData(tree: Tree) = {
			val Apply(_, Literal(Constant(name: String)) :: Literal(Constant(num: Int)) :: Nil) = tree
			new CstmAnt(name, num)
		}

		getClassAnnotation[Test, CstmAnt].map(_.tree) foreach {
			classAnnotationTree =>
				val classAnnotation = getCustomAnnotationData(classAnnotationTree)
				println(classAnnotation)
		}

		getMemberAnnotation[Test, CstmAnt]("ff").map(_.tree) foreach {
			memberAnnotationTree =>
				val memberAnnotation = getCustomAnnotationData(memberAnnotationTree)
				println(memberAnnotation)
		}

		getArgAnnotation[Test, CstmAnt]("mm", "arg").map(_.tree) foreach {
			argAnnotationTree =>
				val argAnnotation = getCustomAnnotationData(argAnnotationTree)
				println(argAnnotation)
		}

	}
}
