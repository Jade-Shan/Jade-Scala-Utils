package jadeutils.comm.dao

import java.lang.RuntimeException

import java.sql.Connection
import java.sql.Savepoint

import jadeutils.common.Logging

import enumeratum.EnumEntry
import enumeratum.Enum
import scala.util.Success
import scala.util.Failure
import scala.util.Try
import scala.xml.dtd.ContentModel.Translator
import java.sql.SQLException
import org.jsoup.select.Evaluator.IsEmpty
import freemarker.core.IfBlock

/**
 * 事务隔离级别的抽象
 */
sealed abstract class TransIso(val id: Int, val name: String) extends EnumEntry
object TransIso extends Enum[TransIso] {
	val values = findValues // mandatory due to Enum extension
	val TransIso = findValues // mandatory due to Enum extension
	case object TS_NONE extends TransIso(Connection.TRANSACTION_NONE, "TRANSACTION_NONE")
	case object TS_READ_COMMITTED extends TransIso(Connection.TRANSACTION_READ_COMMITTED, "TRANSACTION_READ_COMMITTED")
	case object TS_READ_UNCOMMITTED extends TransIso(Connection.TRANSACTION_READ_UNCOMMITTED, "TRANSACTION_READ_UNCOMMITTED")
	case object TS_REPEATABLE_READ extends TransIso(Connection.TRANSACTION_REPEATABLE_READ, "TRANSACTION_REPEATABLE_READ")
	case object TS_SERIALIZABLE extends TransIso(Connection.TRANSACTION_SERIALIZABLE, "TRANSACTION_SERIALIZABLE")
}

/**
 * Transaction Nesting
 */
sealed abstract class TransNesting(val id: Int, val name: String) extends EnumEntry
object TransNesting extends Enum[TransNesting] {
	val values = findValues // mandatory due to Enum extension
	val TransNesting = findValues // mandatory due to Enum extension
	// PROPAGATION_REQUIRED -- 支持当前事务，如果当前没有事务，就新建一个事务。这是最常见的选择。
	case object TS_PG_REQUIRED extends TransNesting(0, "PROPAGATION_REQUIRED")
	// PROPAGATION_SUPPORTS -- 支持当前事务，如果当前没有事务，就以非事务方式执行。
	case object TS_PG_SUPPORTS extends TransNesting(1, "PROPAGATION_SUPPORTS")
	// PROPAGATION_MANDATORY -- 支持当前事务，如果当前没有事务，就抛出异常。
	case object TS_PG_MANDATORY extends TransNesting(2, "PROPAGATION_MANDATORY")
	// PROPAGATION_REQUIRES_NEW -- 新建事务，如果当前存在事务，把当前事务挂起。
	case object TS_PG_REQUIRES_NEW extends TransNesting(3, "PROPAGATION_REQUIRES_NEW")
	// PROPAGATION_NOT_SUPPORTED -- 以非事务方式执行操作，如果当前存在事务，就把当前事务挂起。
	case object TS_PG_NOT_SUPPORTED extends TransNesting(4, "PROPAGATION_NOT_SUPPORTED")
	// PROPAGATION_NEVER -- 以非事务方式执行，如果当前存在事务，则抛出异常。
	case object TS_PG_NEVER extends TransNesting(5, "PROPAGATION_NEVER")
	// PROPAGATION_NESTED -- 如果当前存在事务，则在嵌套事务内执行。如果当前没有事务，则进行与PROPAGATION_REQUIRED类似的操作。
	case object TS_PG_NESTED extends TransNesting(6, "PROPAGATION_NESTED")
}

abstract class TransactionLayer(
	val autoCommit: Boolean,                        //
	val savepoint: Either[Throwable, Savepoint]  //
)

class NewTransactionLayer(savepoint: Either[Throwable, Savepoint])
	extends TransactionLayer(false, savepoint)
{
	
	override def toString(): String = "new-trans,autoCommit=%s".format(autoCommit)

}

class JoinLastTransaction extends TransactionLayer(false, null) {
	
	override def toString(): String = "join-trans,autoCommit=%s".format(autoCommit)

}

class NoTransactionLayer(savepoint: Either[Throwable, Savepoint])
	extends TransactionLayer(true, savepoint) //
{
	def this() = this(null)
	
	override def toString(): String = "no-trans,autoCommit=%s".format(autoCommit)
}

class TransactionStack(val tag: String) extends Logging {	
	
	val name = tag + "-" + System.currentTimeMillis()
	
	var transStack: List[TransactionLayer] = Nil
	
	def current(): Option[TransactionLayer] = {
		if (transStack.isEmpty) None else Some(transStack.head)
	}

	def push(transEntry: TransactionLayer): Unit = {
		logTrace("Trans layer stack before push: {}", transStack)
		transStack = transEntry :: transStack
		logTrace("Trans layer stack after push: {}", transStack)
	}

	def pop(): Option[TransactionLayer] = {
		logTrace("Trans layer stack before pop: {}", transStack)
		val currLayer = current()
		transStack = transStack.tail
		//		if (!isInTransaction) this.close() // TODO: 全部事务完成，关闭会话。这一步要放到Service里做
		logTrace("Trans layer stack after  pop: {}", transStack)
		currLayer
	}

	def isInTransaction() = if (transStack.isEmpty) false else transStack.head match {
		case l: NewTransactionLayer => true
		case l: JoinLastTransaction => true
		case l: NoTransactionLayer  => false
	}

	override def toString = "TransStack:%s[%s]".format(name, transStack.toString())
}

class DataSourcetHolder(val pool: DataSourcePool, val defaultIsolation: TransIso) extends Logging {

	private[this] val resource = new ThreadLocal[Connection]
	
	def dialect(): Dialect = pool.dialect

	def isBroken() = null == resource.get || resource.get.isClosed
	
	def retrunBack() {
		if (null != resource.get) {
			pool.retrunBack(resource.get)
			resource.remove()
		}
	}
	
	def connection(): Try[Connection] = if (isBroken()) {
		retrunBack() 
		val newConn = pool.borrow()
		if (newConn.isSuccess) {
			resource.set(newConn.get)
			newConn
		} else Failure(new SQLException("no db connection"))
	} else Success(resource.get)
	
	private[this] val localTransaction = new ThreadLocal[TransactionStack] {
		override def initialValue(): TransactionStack = new TransactionStack("datasource")
	}
	
	def transaction(): TransactionStack = localTransaction.get
	
	def isInTransaction(): Boolean = transaction().isInTransaction()
}	
	
abstract class BaseTransactionService extends Logging {
	import scala.reflect.runtime.universe.Type
	import scala.reflect.runtime.universe.typeOf
	import scala.reflect.runtime.universe.TypeTag
	import jadeutils.comm.dao.TransNesting._
	import jadeutils.comm.dao.TransIso._

	protected val dataSource: DataSourcetHolder
	
	def transaction = dataSource.transaction()

	@throws(classOf[Throwable])
	def withTransaction[T](nesting: TransNesting, iso: TransIso)(callFunc: => T)(implicit m: TypeTag[T]): T = //
	{ // 隐式参数自动匹配被事务包裹函数的返回类型
		warpSession(nesting, iso, callFunc) match {
			case Success(r) => r
			case Failure(e: Throwable) => throw e
		}
	}

	@throws(classOf[Throwable])
	def withTransaction[T](nesting: TransNesting)(callFunc: => T)(implicit m: TypeTag[T]): T = //
	{ // 隐式参数自动匹配被事务包裹函数的返回类型
		warpSession(nesting, dataSource.defaultIsolation, callFunc) match {
			case Success(r) => r
			case Failure(e: Throwable) => throw e
		}
	}

	@throws(classOf[Throwable])
	def withTransaction[T](iso: TransIso)(callFunc: => T)(implicit m: TypeTag[T]): T = //
	{ // 隐式参数自动匹配被事务包裹函数的返回类型
		warpSession(TS_PG_REQUIRED, iso, callFunc) match {
			case Success(r) => r
			case Failure(e: Throwable) => throw e
		}
	}

	@throws(classOf[Throwable])
	def withTransaction[T](callFunc: => T)(implicit m: TypeTag[T]): T = {
		val transRes = warpSession(TS_PG_REQUIRED, dataSource.defaultIsolation, callFunc) 
		logTrace("before trans end, trans-result is : ", transRes)
		transRes match {
			case Success(r) => r
			case Failure(e: Throwable) => throw e
		}
	}

	private def warpSession[T](nesting: TransNesting, iso: TransIso, callFunc: => T)(implicit m: TypeTag[T]): Try[T] = //
	{
		if (dataSource.isBroken()) {
			logError("Lost Connection from database")
			Left(new RuntimeException("Lost Connection from database"))
		}

		dealwithTransNesting(nesting, iso)   // 新建内层事务
		
//		val callRes: Try[T] = util.Try(callFunc)   // 执行具体操作
		val callRes: Try[T] = try {
			val cfr = callFunc
			Success(cfr)   // 执行具体操作
		} catch {
			case t: Throwable => Failure(t)
		}
			

		endTransNesting(callRes)             // 处理异常并返回到外层事务
	}

	private[this] def endTransNesting[T](callRes: Try[T])(implicit m: TypeTag[T]): Try[T] = {
		val conn = dataSource.connection().get
		logTrace("before trans end, call-func-result is {}: ", callRes)
		val currTransLayer = transaction.pop// 弹出当前一层事务
		logTrace("remove Trans on connection, trans id: {}, conn: {} auto-commit:{} , iso: {}", //
				currTransLayer, dataSource.isBroken(), conn.getAutoCommit)

		val transResult: Try[T] = currTransLayer match {
			case Some(l: NewTransactionLayer) => callRes match {
				case s: Success[T] => { // 新事务，成功后提交修改
					logTrace("Call Func Success, start commit transaction manually: {}", transaction.name)
					conn.commit()
					logTrace("Call Func Success, commit transaction manually success: {}", transaction.name)
					callRes
				}
				case Failure(f) => { // 新事务，失败后直接回滚。不让错误传播到外层
					logTrace("Call Func Err, Trans rollback: S: {} for err: {}", f)
					if (null != l && l.savepoint.isRight) {
						conn.rollback(l.savepoint.right.get)
					} else conn.rollback()
					Success(generateDefaultResult(typeOf[T]).asInstanceOf[T])
				}
			}
			case Some(l: JoinLastTransaction) => callRes match {
				case s: Success[T] => { // 外层事务，成功后不提交，等待外层事务完成一同提交
					logTrace("Call Func Success, retrun outter transaction : S: {}", transaction)
					callRes
				}
				case Failure(f) => { // 外层事务，失败后不回滚，报错给外层事务一同回滚
					logTrace("Call Func Err, need rollback outter transaction : S: {}", transaction)
					callRes
				}
			}
			case Some(_) => callRes match { // 不支持的事务，作为当作外层事务处理
				case s: Success[T] => {
					logTrace("Call Func Success, not in transaction: S: {}", transaction)
					callRes
				}
				case Failure(f) => {
					logTrace("Call Func Err, need rollback outter transaction : S: {}", transaction)
					callRes
				}
			}
			case None => callRes match {
				case s: Success[T] => { // 没有事务，按自动提交操作
					logTrace("Call Func Success, not in transaction: S: {}", transaction)
					if (conn.getAutoCommit) { conn.commit() }
					callRes
				}
				case Failure(f) => {
					logTrace("Call Func Err, not in transaction so no rollback: S: {}", transaction)
					Success(generateDefaultResult(typeOf[T]).asInstanceOf[T])
				}
			}
		}

		logTrace("resume outter layer trans, autosave: {} ", !transaction.isInTransaction)
		conn.setAutoCommit(!transaction.isInTransaction) // 恢复外层事务

		transResult
	}
	

	private[this] def dealwithTransNesting(nesting: TransNesting, iso: TransIso): Unit = {

		def createSavepoint(): Either[Throwable, Savepoint] = Left(//
				new RuntimeException("DB not Support Savepoint")) 
//		try {
//			Right(dataSource.setSavepoint("" + System.currentTimeMillis()))
//		} catch { case e: Throwable => Left(e) }

		logTrace("warpping transaction layer: {}", nesting)
		val currLayer:Try[TransactionLayer] = nesting match {
			case TS_PG_NEVER => if (transaction.isInTransaction) {
				// PROPAGATION_NEVER -- 以非事务方式执行，如果当前存在事务，则抛出异常。
				Failure(new RuntimeException("Expect not in any transaction"))
			} else Success(new NoTransactionLayer)
			case TS_PG_MANDATORY => if (transaction.isInTransaction) {
				// PROPAGATION_MANDATORY -- 支持当前事务，如果当前没有事务，就抛出异常。
				Success(new JoinLastTransaction)
			} else Failure(new RuntimeException("Expect in transaction but not"))
			case TS_PG_REQUIRED => if (transaction.isInTransaction) {
				// PROPAGATION_REQUIRED -- 支持当前事务，如果当前没有事务，就新建一个事务。这是最常见的选择。
				Success(new JoinLastTransaction)
			} else Success(new NewTransactionLayer(createSavepoint))
			case TS_PG_NESTED => {
				// PROPAGATION_NESTED -- 如果当前存在事务，则在嵌套事务内执行。
				// 如果当前没有事务，则进行与PROPAGATION_REQUIRED类似的操作。
				Success(new NewTransactionLayer(createSavepoint))
			}
			case TS_PG_SUPPORTS => if (transaction.isInTransaction) {
				// PROPAGATION_SUPPORTS -- 支持当前事务，如果当前没有事务，就以非事务方式执行。
				Success(new JoinLastTransaction)
			} else Success(new NoTransactionLayer)
			case TS_PG_REQUIRES_NEW => {
				// PROPAGATION_REQUIRES_NEW -- 新建事务，如果当前存在事务，把当前事务挂起。
				Success(new NewTransactionLayer(createSavepoint))
			}
			case TS_PG_NOT_SUPPORTED => {
				// PROPAGATION_NOT_SUPPORTED -- 以非事务方式执行操作，如果当前存在事务，就把当前事务挂起。
				Success(new NoTransactionLayer)
			}
			case _ => Failure(new RuntimeException("UnSupport Transaction Type"))
		}

		if (currLayer.isSuccess) {
			transaction.push(currLayer.get)	
		} else throw currLayer.failed.get 

		val needAutoCommit = !transaction.isInTransaction

		val conn = dataSource.connection().get
		conn.setTransactionIsolation(iso.id)
		conn.setAutoCommit(needAutoCommit)
		logTrace("   add Trans on connection, conn:{}, auto-commit:{}, iso:{}, transaction:{}", //
				dataSource, conn.getAutoCommit, iso, transaction)
	}

	private[this] def generateDefaultResult(m: Type): Any = m match {
		case t if (t <:< typeOf[Byte])       => 0
		case t if (t <:< typeOf[Short])      => 0
		case t if (t <:< typeOf[Int])        => 0
		case t if (t <:< typeOf[Long])       => 0L
		case t if (t <:< typeOf[Float])      => 0F
		case t if (t <:< typeOf[Double])     => 0
		case t if (t <:< typeOf[Char])       => '0'
		case t if (t <:< typeOf[Boolean])    => false
		case t if (t <:< typeOf[Option[_]]) => None
		case t if (t <:< typeOf[Unit])       => ()
		case _ => null
	}

}


