object example02 {

	(("foo" + "bar") + "helloword") == ("foo" + ("bar" + "helloword"))
                                                  //> res0: Boolean = true
	((3 + 2) + 1) == (3 + (2 + 1))            //> res1: Boolean = true
	((3 * 2) * 1) == (3 * (2 * 1))            //> res2: Boolean = true
	((true || false) || true) == (true || (false || true))
                                                  //> res3: Boolean = true
	((true && false) && true) == (true && (false && true))
                                                  //> res4: Boolean = true

	(a: String) => a == (a + "") == ("" + a)  //> res5: String => Boolean = example02$$$Lambda$8/440434003@3d8c7aca
	(n: Int) => n == (n + 0) == (0 + n)       //> res6: Int => Boolean = example02$$$Lambda$9/1020923989@7a5d012c
	(n: Int) => n == (n * 1) == (1 * n)       //> res7: Int => Boolean = example02$$$Lambda$10/1068934215@79b4d0f
	(b: Boolean) => b == (b || false) == (false || b)
                                                  //> res8: Boolean => Boolean = example02$$$Lambda$11/1798286609@79698539
	(b: Boolean) => b == (b && true) == (true || b)
                                                  //> res9: Boolean => Boolean = example02$$$Lambda$12/1945604815@2ed94a8b

	trait SemiGroup[A] {

		def op(a1: A, a2: A): A

	}

	trait Monoid[A] extends SemiGroup[A] {

		def zero: A

	}

	val stringMonoid = new Monoid[String] {

		def op(a1: String, a2: String) = a1 + a2

		def zero = ""
	}                                         //> stringMonoid  : example02.Monoid[String] = example02$$anon$1@38082d64

	val intAdditionMonoid = new Monoid[Int] {

		def op(a1: Int, a2: Int) = a1 + a2

		def zero = 0
	}                                         //> intAdditionMonoid  : example02.Monoid[Int] = example02$$anon$2@dfd3711

	val intMultiplyMonoid = new Monoid[Int] {

		def op(a1: Int, a2: Int) = a1 * a2

		def zero = 1
	}                                         //> intMultiplyMonoid  : example02.Monoid[Int] = example02$$anon$3@42d3bd8b

	val booleanOrMonoid = new Monoid[Boolean] {

		def op(a1: Boolean, a2: Boolean) = a1 || a2

		def zero = false

	}                                         //> booleanOrMonoid  : example02.Monoid[Boolean] = example02$$anon$4@26ba2a48

	val booleanAndMonoid = new Monoid[Boolean] {

		def op(a1: Boolean, a2: Boolean) = a1 && a2

		def zero = true

	}                                         //> booleanAndMonoid  : example02.Monoid[Boolean] = example02$$anon$5@5f2050f6
                                                  //| 
	/* 列表类型与拼接操作的幺半群实现，它的幺元是空列表： */
	def listMonoid[A] = new Monoid[List[A]] {
		def op(a1: List[A], a2: List[A]) = a1 ++ a2

		def zero = Nil
	}                                         //> listMonoid: [A]=> example02.Monoid[List[A]]{def zero: scala.collection.immu
                                                  //| table.Nil.type}

	/* `Option`容器类型与取值操作的幺半群实现，它的幺元是空： */
	def optionMonoid[A] = new Monoid[Option[A]] {

		def op(a1: Option[A], a2: Option[A]) = a1 orElse a2

		def zero = None
	}                                         //> optionMonoid: [A]=> example02.Monoid[Option[A]]{def zero: None.type}

	val words = "Hic" :: "Est" :: "Index" :: Nil
                                                  //> words  : List[String] = List(Hic, Est, Index)

	val s = words.foldLeft(stringMonoid.zero)(stringMonoid.op)
                                                  //> s  : String = HicEstIndex
	val t = words.foldRight(stringMonoid.zero)(stringMonoid.op)
                                                  //> t  : String = HicEstIndex
	s == t == ((("" + "Hic") + "Est") + "Index") == ("" + ("Hic" + ("Est" + "Index")))
                                                  //> res10: Boolean = false

	def concatenate[A](as: List[A], m: Monoid[A]): A = as.foldLeft(m.zero)(m.op)
                                                  //> concatenate: [A](as: List[A], m: example02.Monoid[A])A

	def foldMapV1[A, B](as: List[A], m: Monoid[B])(f: A => B): B = {
		for { e <- as } yield f(e)
	}.foldLeft(m.zero)(m.op)                  //> foldMapV1: [A, B](as: List[A], m: example02.Monoid[B])(f: A => B)B

	// https://github.com/fpinscala/fpinscala
	def foldMapV2[A, B](as: List[A], m: Monoid[B])(f: A => B): B = {
		as.foldLeft(m.zero)((b, a) => m.op(b, f(a)))
	}                                         //> foldMapV2: [A, B](as: List[A], m: example02.Monoid[B])(f: A => B)B

	foldMapV1[Int, String](1 :: 2 :: 3 :: Nil, stringMonoid)("" + _)
                                                  //> res11: String = 123

	foldMapV2[Int, String](1 :: 2 :: 3 :: Nil, stringMonoid)("" + _)
                                                  //> res12: String = 123

	def endoMonoid[A]: Monoid[A => A] = new Monoid[A => A] {
		def op(f: A => A, g: A => A) = f compose g
		val zero = (a: A) => a
	}                                         //> endoMonoid: [A]=> example02.Monoid[A => A]

	def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B): B =
		foldMapV2(as, endoMonoid[B])(f.curried)(z)
                                                  //> foldRight: [A, B](as: List[A])(z: B)(f: (A, B) => B)B

	// We can get the dual of any monoid just by flipping the `op`.
	def dual[A](m: Monoid[A]): Monoid[A] = new Monoid[A] {
		def op(x: A, y: A): A = m.op(y, x)
		val zero = m.zero
	}                                         //> dual: [A](m: example02.Monoid[A])example02.Monoid[A]

	// Folding to the left is the same except we flip the arguments to
	// the function `f` to put the `B` on the correct side.
	// Then we have to also "flip" the monoid so that it operates from left to right.
	def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B): B =
		foldMapV2(as, dual(endoMonoid[B]))(a => b => f(b, a))(z)
                                                  //> foldLeft: [A, B](as: List[A])(z: B)(f: (B, A) => B)B

	def foldMapV[A, B](as: IndexedSeq[A], m: Monoid[B])(f: A => B): B =
		if (as.length == 0)
			m.zero
		else if (as.length == 1)
			f(as(0))
		else {
			val (l, r) = as.splitAt(as.length / 2)
			m.op(foldMapV(l, m)(f), foldMapV(r, m)(f))
		}                                 //> foldMapV: [A, B](as: IndexedSeq[A], m: example02.Monoid[B])(f: A => B)B
}