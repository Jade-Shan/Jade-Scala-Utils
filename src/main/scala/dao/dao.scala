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
import jadeutils.database.orm.Column
import java.sql.PreparedStatement
import scala.math.BigDecimal
import java.util.Date

trait Dao[T <: Record[K], K] {

  def queryModel(sql: String): Seq[T]

  def queryModel(sql: String, colNames: Set[String]): Seq[T]

  def queryModel(sql: String, values: Seq[Any]): Seq[T]

  def queryModel(query: String, params: Map[String, AnyRef]): Seq[T]

  def queryModel(sql: String, colNames: Set[String], values: Seq[Any]): Seq[T]

  def queryModel(query: String, colNames: Set[String], params: Map[String, AnyRef]): Seq[T]

  def query(sql: String): List[Map[String, AnyRef]]

  def query(sql: String, colNames: Set[String]): List[Map[String, AnyRef]]

  def query(sql: String, values: Seq[Any]): List[Map[String, AnyRef]]

  def query(query: String, params: Map[String, AnyRef]): List[Map[String, AnyRef]]
  
  def query(query: String, colNames: Set[String], params: Map[String, Object]): List[Map[String, Object]]

  def query(sql: String, colNames: Set[String], values: Seq[Any]): List[Map[String, AnyRef]]

  def getById(id: K): Try[T]

  def executeUpdate(sql: String, params: Map[String, AnyRef]): Try[Int]

  def insert(model: T): Try[Unit]

  def update(model: T): Try[Unit]

}

abstract class JDBCTemplateDao[T <: Record[K], K](datasource: DataSourcetHolder) extends Dao[T, K] {

  val genType: Type = this.getClass().getGenericSuperclass()
  val paramType: ParameterizedType = genType.asInstanceOf[ParameterizedType]
  val params: Array[Type] = paramType.getActualTypeArguments()
  val entryClass: Class[T] = params(0).asInstanceOf[Class[T]]

  def queryModel(sql: String): Seq[T] = {
    queryModel(sql, Set.empty[String], Map.empty[String, Any])
  }

  def queryModel(sql: String, colNames: Set[String]): Seq[T] = {
    queryModel(sql, colNames, Map.empty[String, Any])
  }

  def queryModel(sql: String, values: Seq[Any]): Seq[T] = {
    queryModel(sql, Set.empty[String], values)
  }

  def queryModel(query: String, params: Map[String, Any]): Seq[T] = {
    queryModel(query, Set.empty[String], params)
  }

  def queryModel(sql: String, colNames: Set[String], values: Seq[Any]): Seq[T] = {
    val conn = datasource.connection()
    val ps = ORMUtil.setQueryValues( //
      conn.get.prepareStatement(sql), values)
    val rs = ps.executeQuery()
    if (!datasource.isInTransaction()) {
      // close jdbc connection if transaction is over
      datasource.retrunBack()
    }
    parseAllRow(colNames, rs)
  }

  def queryModel( //
    query: String, colNames: Set[String], params: Map[String, Any] //
  ): Seq[T] = {
    val values = ORMUtil.parseValues(query, params)
    val sql = ORMUtil.parseQuery(query)
    val conn = datasource.connection()
    val ps = ORMUtil.setQueryValues( //
      conn.get.prepareStatement(sql), values)
    val rs = ps.executeQuery()
    if (!datasource.isInTransaction()) {
      // close jdbc connection if transaction is over
      datasource.retrunBack()
    }
    parseAllRow(colNames, rs)
  }

  def parseAllRow(colNames: Set[String], rs: ResultSet): Seq[T] = {
    var ll: List[T] = Nil
    while (rs.next()) {
      ll = ORMUtil.row2Obj[T, K](entryClass, colNames, rs) :: ll
    }
    ll.reverse
  }

  def query(sql: String): List[Map[String, AnyRef]] = {
    query(sql, Set.empty[String])
  }

  def query(sql: String, colNames: Set[String]): List[Map[String, AnyRef]] = {
    query(sql, colNames, Seq.empty[Any])
  }

  def query(sql: String, values: Seq[Any]): List[Map[String, AnyRef]] = {
    query(sql, Set.empty[String], values)
  }

  def query(query: String, params: Map[String, AnyRef]): List[Map[String, AnyRef]] = {
    this.query(query, Set.empty[String], params)
  }

  def query( //
    sql: String, colNames: Set[String], values: Seq[Any] //
  ): List[Map[String, AnyRef]] = {
    var ll: List[Map[String, AnyRef]] = Nil
    val conn = datasource.connection()
    val ps = ORMUtil.setQueryValues( //
      conn.get.prepareStatement(sql), values)
    val rs = ps.executeQuery()
    while (rs.next()) {
      ORMUtil.row2map(colNames, rs) :: ll
    }
    if (!datasource.isInTransaction()) {
      // close jdbc connection if transaction is over
      datasource.retrunBack()
    }
    ll.reverse.toList
  }

  def query( //
    query: String, colNames: Set[String], params: Map[String, AnyRef] //
  ): List[Map[String, AnyRef]] = {
    var ll: List[Map[String, AnyRef]] = Nil
    val values = ORMUtil.parseValues(query, params)
    val sql = ORMUtil.parseQuery(query)
    val conn = datasource.connection()
    val ps = ORMUtil.setQueryValues( //
      conn.get.prepareStatement(sql), values)
    val rs = ps.executeQuery()
    while (rs.next()) {
      ORMUtil.row2map(colNames, rs) :: ll
    }
    if (!datasource.isInTransaction()) {
      // close jdbc connection if transaction is over
      datasource.retrunBack()
    }
    ll.reverse.toList
  }

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

}

object ORMUtil {
  import scala.collection.mutable.{ Map => MMap }
  import scala.collection.mutable.{ Seq => MSeq }

  def allRow2map(colNames: Set[String], rs: ResultSet): Seq[Map[String, AnyRef]] = {
    var lst: List[Map[String, AnyRef]] = Nil
    while (rs.next()) { lst = row2map(colNames, rs) :: lst }
    lst.reverse
  }

  def row2map(colNames: Set[String], rs: ResultSet): Map[String, AnyRef] = {
    val map: MMap[String, AnyRef] = MMap.empty
    for (name <- colNames) {
      try {
        map.put(name, rs.getObject(name));
      } catch { case e: SQLException => e.printStackTrace() }
    }
    map.toMap
  }

  def allRow2obj[T <: Record[K], K](clazz: Class[T], colNames: Set[String], rs: ResultSet): Seq[T] = {
    var lst: List[T] = Nil
    while (rs.next()) { lst = row2Obj[T, K](clazz, colNames, rs) :: lst }
    lst.reverse
  }

  def row2Obj[T <: Record[K], K](clazz: Class[T], colNames: Set[String], rs: ResultSet): T = {
    val obj = clazz.getDeclaredConstructor(clazz).newInstance();
    for (f <- clazz.getDeclaredFields()) {
      val clm: Column = f.getAnnotation(classOf[Column])
      val colName = if (null == clm || StringUtils.isBlank(clm.column())) {
        f.getName
      } else clm.column()
      if (null != colNames && !colNames.isEmpty && !colNames.contains(colName)) {
        // skip this column
      } else {
        try {
          val colValue = rs.getObject(colName)
          f.setAccessible(true);
          f.set(obj, colValue);
        } catch {
          case e: SQLException             => e.printStackTrace()
          case e: IllegalArgumentException => e.printStackTrace()
          case e: IllegalAccessException   => e.printStackTrace()
        }
      }
    }
    obj
  }

  val paramRegex = """:([-_0-9a-zA-Z]+)""".r

  def parseValues(query: String, params: Map[String, Any]): Seq[Any] = {
    for (m <- paramRegex findAllMatchIn query) yield m.group(1)
  }.toSeq

  /* 替换查询语句中以冒号开头的参数名替换为sql标准中的问号参数 */
  def parseQuery(query: String): String = paramRegex.replaceAllIn(query, "?")

  /* 设置sql中问号参数的值 */
  def setQueryValues(ps: PreparedStatement, values: Seq[Any]): PreparedStatement = {
    for (n <- 1 to values.size) {
      values(n) match {
        case o: String               => ps.setString(n, o)
        case o: Short                => ps.setShort(n, o)
        case o: Int                  => ps.setInt(n, o)
        case o: Long                 => ps.setLong(n, o)
        case o: java.math.BigDecimal => ps.setBigDecimal(n, o)
        case o: BigDecimal           => ps.setBigDecimal(n, o.bigDecimal)
        case o: java.sql.Date        => ps.setDate(n, o)
        case o: Date                 => ps.setDate(n, new java.sql.Date(o.getTime))
        case _                       => ps.setObject(n, null)
      }
    }
    ps
  }

}
