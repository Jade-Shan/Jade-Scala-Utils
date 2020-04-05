package jadeutils.comm.dao

trait Dao[T, K] {

	def getById(id: K): Option[T]

	def insert(model: T): Unit

}
