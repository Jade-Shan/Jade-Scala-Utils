package jadeutils.comm.dao

import java.lang.Class
import java.sql.Connection
import scala.util.Try
import java.sql.ResultSet
import java.lang.reflect.ParameterizedType
import java.lang.reflect.Type
import java.sql.SQLException
import org.apache.commons.lang.StringUtils

trait Dao[T, K] {

	def getById(id: K): Option[T]

	def insert(model: T): Unit

}


abstract class JDBCTemplateDao[T, K](datasource: DataSourcetHolder) extends Dao[T, K] {
	import jadeutils.database.orm.Column
	
	val genType: Type   = this.getClass().getGenericSuperclass()
	val paramType: ParameterizedType = genType.asInstanceOf[ParameterizedType]
	val params: Array[Type] = paramType.getActualTypeArguments()
	val entryClass: Class[T] = params(0).asInstanceOf[Class[T]]
	
	def doQuery(sql: String): Seq[T] = doQuery(sql, Map.empty)
	
	def doQuery(sql: String, params: Map[String, AnyRef]): Seq[T] = {
		val conn = datasource.connection()
		val ps = conn.get.prepareStatement(sql)
		val rs = ps.executeQuery()
		parseAllRow(rs)
	}
	
	def parseAllRow(rs: ResultSet): Seq[T] = {
		val ll: List[T] = Nil
		while (rs.next()) {
		  parseRow(rs) :: ll
		}
		ll
	}

	def parseRow(rs: ResultSet): T = {
		val obj = entryClass.getDeclaredConstructor(entryClass).newInstance()
		val fields = entryClass.getDeclaredFields()
		for (f <- fields) {
			val clm = f.getAnnotation(classOf[Column])
			if (clm != null) {
				val cn = clm.column()
				val colName =
					if (!StringUtils.isBlank(cn)) cn else f.getName()
				try {
					val colIdx = rs.findColumn(colName);
					val colValue = rs.getObject(colIdx);
					f.setAccessible(true);
					f.set(obj, colValue);
				} catch {
					case e: SQLException => e.printStackTrace()
					case e: IllegalArgumentException => e.printStackTrace()
					case e: IllegalAccessException => e.printStackTrace()
				}
			}
		}
		obj
	}

}