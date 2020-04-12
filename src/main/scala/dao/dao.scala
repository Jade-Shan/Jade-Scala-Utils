package jadeutils.comm.dao

import java.lang.Class
import java.sql.Connection
import java.sql.ResultSet
import java.lang.reflect.ParameterizedType
import java.lang.reflect.Type
import java.sql.SQLException

import scala.util.Try
import scala.util.Failure
import scala.util.Success

import org.apache.commons.lang.StringUtils

import jadeutils.database.orm.Record
import jadeutils.database.orm.ORMUtil

trait Dao[T <: Record[K], K] {

	def getById(id: K): Option[T]

	def insert(model: T): Try[Unit]
	
}


abstract class JDBCTemplateDao[T <: Record[K], K](datasource: DataSourcetHolder) extends Dao[T, K] {
	
	val genType: Type   = this.getClass().getGenericSuperclass()
	val paramType: ParameterizedType = genType.asInstanceOf[ParameterizedType]
	val params: Array[Type] = paramType.getActualTypeArguments()
	val entryClass: Class[T] = params(0).asInstanceOf[Class[T]]
	
	def executeUpdate(sql: String, params: Map[String, AnyRef]): Try[Int] = {
		val conn = datasource.connection()
		val ps = conn.get.prepareStatement(sql)
    val result = try {
      Success(ps.executeUpdate())
    } catch { case e: Exception => Failure(e) }
    if (!datasource.isInTransaction()) {
      // close jdbc connection if transaction is over
      datasource.retrunBack()
    }
    result
	}
	
	def queryModel(sql: String): Seq[T] = queryModel(sql, Map.empty)
	
	def queryModel(sql: String, params: Map[String, AnyRef]): Seq[T] = {
		val conn = datasource.connection()
		val ps = conn.get.prepareStatement(sql)
		val rs = ps.executeQuery()
    if (!datasource.isInTransaction()) {
      // close jdbc connection if transaction is over
      datasource.retrunBack()
    }
    parseAllRow(rs)
	}
	
	def parseAllRow(rs: ResultSet): Seq[T] = {
		var ll: List[T] = Nil
		while (rs.next()) {
		  val record = ORMUtil.result2record[T, K](entryClass, rs)
		  ll = record :: ll
		}
		ll.reverse
	}
	
	def query(colNames: Array[String], sql: String, 
	    params: Map[String, AnyRef]): List[Map[String, Any]] = //
	{
	  var ll: List[Map[String, Any]] = Nil
		val conn = datasource.connection()
		val ps = conn.get.prepareStatement(sql)
		val rs = ps.executeQuery()
		while (rs.next()) {
		  val record = result2map(colNames, rs)
		  ll = record :: ll
		}
    if (!datasource.isInTransaction()) {
      // close jdbc connection if transaction is over
      datasource.retrunBack()
    }
		ll
	}
	
	def result2map(colNames: Array[String], rs: ResultSet): Map[String, Any] = {
	  var map: Map[String, Any] = Map.empty
	  for (n <- colNames) {
	    map = map + (n -> rs.getObject(n))
	  }
	  map
	}

}
