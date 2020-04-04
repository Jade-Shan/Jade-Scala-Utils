package jadeutils.comm.dao

trait Dao[T, K] {

	def getById(id: K): Either[RuntimeException, T]

	def insert(model: T): Either[RuntimeException, Unit]

}
