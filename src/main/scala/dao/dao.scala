package jadeutils.comm.dao

import java.lang.Class
import java.sql.Connection
import scala.util.Try
import java.sql.ResultSet
import java.lang.reflect.ParameterizedType
import java.lang.reflect.Type
import java.sql.SQLException
import org.apache.commons.lang.StringUtils
import jadeutils.database.orm.ORMUtil
import jadeutils.database.orm.Record

trait Dao[T <: Record[K], K] {

	def getById(id: K): Option[T]

	def insert(model: T): Unit

}


abstract class JDBCTemplateDao[T <: Record[K], K](datasource: DataSourcetHolder) extends Dao[T, K] {
	
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
//		  ORMUtil.result2record(arg0, arg1)
		  ???
		}
		ll
	}

}

//object ORMUtil {
//  
//	import jadeutils.database.orm.Column
//
//	def parseRow[T](entryClass: Class[T], rs: ResultSet): T = {
//		val obj = entryClass.getDeclaredConstructor(entryClass).newInstance()
//		val fields = entryClass.getDeclaredFields()
//		for (f <- fields) {
//			val clm = f.getAnnotation(classOf[Column])
//			if (clm != null) {
//				val cn = clm.column()
//				val colName =
//					if (!StringUtils.isBlank(cn)) cn else f.getName()
//				try {
//					val colIdx = rs.findColumn(colName);
//					val colValue = rs.getObject(colIdx);
//					f.setAccessible(true);
//					f.set(obj, colValue);
//				} catch {
//					case e: SQLException => e.printStackTrace()
//					case e: IllegalArgumentException => e.printStackTrace()
//					case e: IllegalAccessException => e.printStackTrace()
//				}
//			}
//		}
//		obj
//	}
//}