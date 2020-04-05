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
	
	override def toString(): String = "new-trans-auto-commit:%s".format(autoCommit)

}

class JoinLastTransaction extends TransactionLayer(false, null) {
	
	override def toString(): String = "join-trans-auto-commit:%s".format(autoCommit)

}

class NoTransactionLayer(savepoint: Either[Throwable, Savepoint])
	extends TransactionLayer(true, savepoint) //
{
	def this() = this(null)
	
	override def toString(): String = "no-trans-auto-commit:%s".format(autoCommit)
}

class DaoSession(val id: String, val conn: Connection, pool: DaoSessionPool //
) extends Logging //
{
	private[this] var transStack: List[TransactionLayer] = Nil

	private[this] def currTransactionLayer(): Option[TransactionLayer] = {
		if (transStack.isEmpty) None else Some(transStack.head)
	}

	def pushTransaction(transEntry: TransactionLayer) {
		logDebug("Trans layer stack before push: {}", transStack)
		transStack = transEntry :: transStack
		logDebug("Trans layer stack after push: {}", transStack)
	}

	def popTransaction(): Option[TransactionLayer] = {
		logDebug("Trans layer stack before pop: {}", transStack)
		val transEntry = currTransactionLayer
		transStack = transStack.tail
		if (!isInTransaction) this.close() // 全部事务完成，关闭会话
		logDebug("Trans layer stack after  pop: {}", transStack)
		transEntry
	}

	def isBroken() = conn.isClosed

	def isInTransaction() = if (transStack.isEmpty) false else transStack.head match {
		case l: NewTransactionLayer => true
		case l: JoinLastTransaction => true
		case l: NoTransactionLayer  => false
	}

	def close() { pool.returnBack(this) }

	override def toString = "(%s, %b)".format(id, isBroken)
}

abstract class DaoSessionPool(val minPoolSize: Int, val maxPoolSize: Int, val initPoolSize: Int //
) extends Logging //
{
	import jadeutils.comm.dao.TransIso.TransIso
	val defaultIsolation: TransIso

	private[this] var idleSess = List[DaoSession]()
	private[this] var actvSess = Map[String, DaoSession]()
	private[this] def size() = idleSess.size + actvSess.size
	private[this] var currSess = new ThreadLocal[DaoSession]

	def this() = this(20, 50, 20)

	// 创建JDBC连接
	protected[this] def connectDB(): Either[RuntimeException, java.sql.Connection]

	def current: Either[RuntimeException, DaoSession] = {
		var s = currSess.get
		if (null == s) borrow() else if (!s.isBroken()) Right(s) else {
			currSess.get.close
			borrow()
		}
	}

	/**
	 * 从连接池中拿一个可用的会话
	 */
	def borrow(): Either[RuntimeException, DaoSession] = {
		def getAvaliable(): Either[RuntimeException, DaoSession] = {
			val sess = if (idleSess.size < 1) {
				// 没有空闲的连接就新建一个
				val dbConn = connectDB()
				if (dbConn.isLeft) Left(dbConn.left.get)
				else Right(new DaoSession("" + size, dbConn.right.get, this))
			} else {
				// 有空闲的连接就取一个
				var first = idleSess.head
				idleSess = idleSess.tail
				Right(first)
			}
			// drop borken session, find next idle session
			if (sess.isRight && !sess.right.get.isBroken()) sess else getAvaliable()
		}
		if (size >= maxPoolSize) {
			Left(new RuntimeException("Db connection Pool filled"))
		} else {
			val pse = getAvaliable() // 取一个可用的连接
			if (pse.isLeft) pse else {
				val sess = pse.right.get
				actvSess = actvSess + (sess.id -> sess)
				currSess.set(sess)

				logDebug("after create session: size: {} ----- max: {}\nidle: {}\nactive: {}",
				size, maxPoolSize, idleSess, actvSess
				)
				Right(sess)
			}
		}
	}

	/**
	 * 查询完成，放回连接池
	 */
	def returnBack(sess: DaoSession) {
		if (actvSess.contains(sess.id) && !actvSess.get(sess.id).get.isInTransaction()) {
			actvSess = actvSess - sess.id
			idleSess = sess :: idleSess
			currSess.remove()
		}
		logDebug("after close session: size: {} ----- max: {}\nidle: {}\nactive: {}",
		size, maxPoolSize, idleSess, actvSess
		)
	}

}

abstract class BaseTransactionService extends Logging {
	import scala.reflect.runtime.universe.Type
	import scala.reflect.runtime.universe.typeOf
	import scala.reflect.runtime.universe.TypeTag
	import jadeutils.comm.dao.TransNesting._
	import jadeutils.comm.dao.TransIso._

	protected val daoSessPool: DaoSessionPool

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
		warpSession(nesting, daoSessPool.defaultIsolation, callFunc) match {
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
		val transRes = warpSession(TS_PG_REQUIRED, daoSessPool.defaultIsolation, callFunc) 
		logDebug("before trans end, trans-result is : ", transRes)
		transRes match {
			case Success(r) => r
			case Failure(e: Throwable) => throw e
		}
	}

	private def warpSession[T](nesting: TransNesting, iso: TransIso, callFunc: => T)(implicit m: TypeTag[T]): Try[T] = //
	{
		val sessT = daoSessPool.current
		if (sessT.isLeft) {
			logError("Lost Connection from database")
			Left(new RuntimeException("Lost Connection from database"))
		}
		val sess = sessT.right.get

		dealwithTransNesting(sess, nesting, iso)   // 新建内层事务
		
//		val callRes: Try[T] = util.Try(callFunc)   // 执行具体操作
		val callRes: Try[T] = try {
			val cfr = callFunc
			Success(cfr)   // 执行具体操作
		} catch {
			case t: Throwable => Failure(t)
		}
			

		endTransNesting(sess, callRes)             // 处理异常并返回到外层事务
	}

	private[this] def endTransNesting[T](sess: DaoSession, callRes: Try[T])(implicit m: TypeTag[T]): Try[T] = {
		logDebug("before trans end, call-func-result is {}: ", callRes)
		logDebug("remove Trans on connection, trans id: {}, conn: {} auto-commit:{} , iso: {}", sess.id, sess.conn, sess.conn.getAutoCommit)
		//
		val currTransLayer = sess.popTransaction() // 弹出当前一层事务

		val transResult: Try[T] = currTransLayer match {
			case Some(l: NewTransactionLayer) => callRes match {
				case s: Success[T] => { // 新事务，成功后提交修改
					logDebug("Call Func Success, start commit transaction manually: S: {}", sess.id)
					sess.conn.commit()
					logDebug("Call Func Success, commit transaction manually success: S: {}", sess.id)
					callRes
				}
				case Failure(f) => { // 新事务，失败后直接回滚。不让错误传播到外层
					logDebug("Call Func Err, Trans rollback: S: {} for err: {}", f)
					if (null != l && l.savepoint.isRight) {
						sess.conn.rollback(l.savepoint.right.get)
					} else sess.conn.rollback()
					Success(generateDefaultResult(typeOf[T]).asInstanceOf[T])
				}
			}
			case Some(l: JoinLastTransaction) => callRes match {
				case s: Success[T] => { // 外层事务，成功后不提交，等待外层事务完成一同提交
					logDebug("Call Func Success, retrun outter transaction : S: {}", sess.id)
					callRes
				}
				case Failure(f) => { // 外层事务，失败后不回滚，报错给外层事务一同回滚
					logDebug("Call Func Err, need rollback outter transaction : S: {}", sess.id)
					callRes
				}
			}
			case Some(_) => callRes match { // 不支持的事务，作为当作外层事务处理
				case s: Success[T] => {
					logDebug("Call Func Success, not in transaction: S: {}", sess.id)
					callRes
				}
				case Failure(f) => {
					logDebug("Call Func Err, need rollback outter transaction : S: {}", sess.id)
					callRes
				}
			}
			case None => callRes match {
				case s: Success[T] => { // 没有事务，按自动提交操作
					logDebug("Call Func Success, not in transaction: S: {}", sess.id)
					if (!sess.conn.getAutoCommit) { sess.conn.commit() }
					callRes
				}
				case Failure(f) => {
					logDebug("Call Func Err, not in transaction so no rollback: S: {}", sess.id)
					Success(generateDefaultResult(typeOf[T]).asInstanceOf[T])
				}
			}
		}

		logDebug("resume outter layer trans, autosave: {} ", !sess.isInTransaction)
		sess.conn.setAutoCommit(!sess.isInTransaction) // 恢复外层事务

		transResult
	}
	

	private[this] def dealwithTransNesting(sess: DaoSession, nesting: TransNesting, iso: TransIso): Unit = {

		def createSavepoint(): Either[Throwable, Savepoint] = Left(//
				new RuntimeException("DB not Support Savepoint")) 
//		try {
//			Right(sess.conn.setSavepoint("" + System.currentTimeMillis()))
//		} catch { case e: Throwable => Left(e) }

		logDebug("warpping transaction layer: {}", nesting)
		val currLayer:Try[TransactionLayer] = nesting match {
			case TS_PG_NEVER => if (sess.isInTransaction) {
				// PROPAGATION_NEVER -- 以非事务方式执行，如果当前存在事务，则抛出异常。
				Failure(new RuntimeException("Expect not in any transaction"))
			} else Success(new NoTransactionLayer)
			case TS_PG_MANDATORY => if (sess.isInTransaction) {
				// PROPAGATION_MANDATORY -- 支持当前事务，如果当前没有事务，就抛出异常。
				Success(new JoinLastTransaction)
			} else Failure(new RuntimeException("Expect in transaction but not"))
			case TS_PG_REQUIRED => if (sess.isInTransaction) {
				// PROPAGATION_REQUIRED -- 支持当前事务，如果当前没有事务，就新建一个事务。这是最常见的选择。
				Success(new JoinLastTransaction)
			} else Success(new NewTransactionLayer(createSavepoint))
			case TS_PG_NESTED => {
				// PROPAGATION_NESTED -- 如果当前存在事务，则在嵌套事务内执行。
				// 如果当前没有事务，则进行与PROPAGATION_REQUIRED类似的操作。
				Success(new NewTransactionLayer(createSavepoint))
			}
			case TS_PG_SUPPORTS => if (sess.isInTransaction) {
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
			sess.pushTransaction(currLayer.get)	
		} else throw currLayer.failed.get 

		val isAutoCommit = !sess.isInTransaction
		sess.conn.setTransactionIsolation(iso.id)
		sess.conn.setAutoCommit(isAutoCommit)
		logDebug("   add Trans on connection, trans id: {}, conn: {} auto-commit:{} , iso: {}", sess.id, sess.conn, sess.conn.getAutoCommit, iso)
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


