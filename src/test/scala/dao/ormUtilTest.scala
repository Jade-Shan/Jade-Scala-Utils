package jadeutils.comm.dao

import org.scalatest.junit.JUnitRunner
import org.scalatest.FunSuite
import org.junit.runner.RunWith

import jadeutils.common.Logging

import java.sql.SQLException
import java.util.Date

@RunWith(classOf[JUnitRunner])
class ORMUtilTest extends FunSuite with Logging {
	
	
	val userTableColumns = Set("last_change_time", "create_time", "id", "name")

	test("Test-DBTable-Annotation") {
		logDebug("------------------------")
		val tablename = ORMUtil.getTableName[User, String](classOf[User])
		assert(tablename.isSuccess)
		assert("`db-test-01.db`.`testuser`" == tablename.get)
		logDebug("------------------------obj 2 map : {}", tablename)
	}

	test("Test-DBColumn-Annotation") {
		val user = new User("1", "Jade")
		val clazz = classOf[User]
		for (f <- clazz.getDeclaredFields()) {
			val clm: Column = f.getAnnotation(classOf[Column])
			logDebug("field ann is: {}", clm)
		}
		//
		val cols = ORMUtil.getColumns[User, String](classOf[User])
		println(cols)
		// 执行结果不应该含有标准答案中没有的元素
		for (c <- cols) assert(userTableColumns.contains(c))
		// 执行结果应该含有标准答案中所有的元素
		for (c <- userTableColumns) assert(cols.contains(c))
	}
	
	test("Test-obj-to-keyValue") {
		val now = new Date(System.currentTimeMillis())
		val user = new User("1", "Jade", now, now)
		val clazz = classOf[User]
		val seq  = ORMUtil.obj2kv[User, String](clazz, user)
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
	
	test("Test-reflect-obj") {
		val cons = classOf[User].getDeclaredConstructors
		println(cons.size)
		for (con <- cons) println("ccccc " + con.getParameterTypes)
		for (con <- cons) println("dddddd " + con.getParameters)
		val con = classOf[User].getDeclaredConstructor(Seq.empty[Class[_]]: _*)
		println(con)
		println(con.newInstance())
	}
	
	test("Test-resultset-to-obj") {
		SqliteEnv.testInEnv(() => {
			val now = new java.sql.Date(System.currentTimeMillis())
			val prep1 = SqliteDataSourceHolder.connection.get.prepareStatement( //
				"insert into " + SqliteEnv.tableName + " values (?, ?, ?, ?)"
			)
			prep1.setString(1, "1");
			prep1.setString(2, "Jade");
			prep1.setDate(3, now);
			prep1.setDate(4, now);
			prep1.addBatch();
			prep1.executeBatch()
			//
			val prep2 = SqliteDataSourceHolder.connection.get.prepareStatement(//
					"select * from " + SqliteEnv.tableName + " where id = ? ")
			prep2.setString(1, "1");
			val rs = prep2.executeQuery()
			val rec = ORMUtil.row2obj[User, String](classOf[User], null, rs)
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
