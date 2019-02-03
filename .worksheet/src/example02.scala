object example02 {;import org.scalaide.worksheet.runtime.library.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(88); val res$0 = 

	(("foo" + "bar") + "helloword") == ("foo" + ("bar" + "helloword"));System.out.println("""res0: Boolean = """ + $show(res$0));$skip(32); val res$1 = 
	((3 + 2) + 1) == (3 + (2 + 1));System.out.println("""res1: Boolean = """ + $show(res$1));$skip(32); val res$2 = 
	((3 * 2) * 1) == (3 * (2 * 1));System.out.println("""res2: Boolean = """ + $show(res$2));$skip(56); val res$3 = 
	((true || false) || true) == (true || (false || true));System.out.println("""res3: Boolean = """ + $show(res$3));$skip(56); val res$4 = 
	((true && false) && true) == (true && (false && true));System.out.println("""res4: Boolean = """ + $show(res$4));$skip(44); val res$5 = 

	(a: String) => a == (a + "") == ("" + a);System.out.println("""res5: String => Boolean = """ + $show(res$5));$skip(37); val res$6 = 
	(n: Int) => n == (n + 0) == (0 + n);System.out.println("""res6: Int => Boolean = """ + $show(res$6));$skip(37); val res$7 = 
	(n: Int) => n == (n * 1) == (1 * n);System.out.println("""res7: Int => Boolean = """ + $show(res$7));$skip(51); val res$8 = 
	(b: Boolean) => b == (b || false) == (false || b);System.out.println("""res8: Boolean => Boolean = """ + $show(res$8));$skip(49); val res$9 = 
	(b: Boolean) => b == (b && true) == (true || b)

	trait SemiGroup[A] {

		def op(a1: A, a2: A): A

	}

	trait Monoid[A] extends SemiGroup[A] {

		def zero: A

	};System.out.println("""res9: Boolean => Boolean = """ + $show(res$9));$skip(229); 

	val stringMonoid = new Monoid[String] {

		def op(a1: String, a2: String) = a1 + a2

		def zero = ""
	};System.out.println("""stringMonoid  : example02.Monoid[String] = """ + $show(stringMonoid ));$skip(104); 

	val intAdditionMonoid = new Monoid[Int] {

		def op(a1: Int, a2: Int) = a1 + a2

		def zero = 0
	};System.out.println("""intAdditionMonoid  : example02.Monoid[Int] = """ + $show(intAdditionMonoid ));$skip(104); 

	val intMultiplyMonoid = new Monoid[Int] {

		def op(a1: Int, a2: Int) = a1 * a2

		def zero = 1
	};System.out.println("""intMultiplyMonoid  : example02.Monoid[Int] = """ + $show(intMultiplyMonoid ));$skip(121); 

	val booleanOrMonoid = new Monoid[Boolean] {

		def op(a1: Boolean, a2: Boolean) = a1 || a2

		def zero = false

	};System.out.println("""booleanOrMonoid  : example02.Monoid[Boolean] = """ + $show(booleanOrMonoid ));$skip(121); 

	val booleanAndMonoid = new Monoid[Boolean] {

		def op(a1: Boolean, a2: Boolean) = a1 && a2

		def zero = true

	};System.out.println("""booleanAndMonoid  : example02.Monoid[Boolean] = """ + $show(booleanAndMonoid ));$skip(144); 
	/* 列表类型与拼接操作的幺半群实现，它的幺元是空列表： */
	def listMonoid[A] = new Monoid[List[A]] {
		def op(a1: List[A], a2: List[A]) = a1 ++ a2

		def zero = Nil
	};System.out.println("""listMonoid: [A]=> example02.Monoid[List[A]]{def zero: scala.collection.immutable.Nil.type}""");$skip(167); 

	/* `Option`容器类型与取值操作的幺半群实现，它的幺元是空： */
	def optionMonoid[A] = new Monoid[Option[A]] {

		def op(a1: Option[A], a2: Option[A]) = a1 orElse a2

		def zero = None
	};System.out.println("""optionMonoid: [A]=> example02.Monoid[Option[A]]{def zero: None.type}""");$skip(48); 

	val words = "Hic" :: "Est" :: "Index" :: Nil;System.out.println("""words  : List[String] = """ + $show(words ));$skip(62); 

	val s = words.foldLeft(stringMonoid.zero)(stringMonoid.op);System.out.println("""s  : String = """ + $show(s ));$skip(61); 
	val t = words.foldRight(stringMonoid.zero)(stringMonoid.op);System.out.println("""t  : String = """ + $show(t ));$skip(84); val res$10 = 
	s == t == ((("" + "Hic") + "Est") + "Index") == ("" + ("Hic" + ("Est" + "Index")));System.out.println("""res10: Boolean = """ + $show(res$10));$skip(80); 

	def concatenate[A](as: List[A], m: Monoid[A]): A = as.foldLeft(m.zero)(m.op);System.out.println("""concatenate: [A](as: List[A], m: example02.Monoid[A])A""");$skip(123); 

	def foldMapV1[A, B](as: List[A], m: Monoid[B])(f: A => B): B = {
		for { e <- as } yield f(e)
	}.foldLeft(m.zero)(m.op);System.out.println("""foldMapV1: [A, B](as: List[A], m: example02.Monoid[B])(f: A => B)B""");$skip(161); 

	// https://github.com/fpinscala/fpinscala
	def foldMapV2[A, B](as: List[A], m: Monoid[B])(f: A => B): B = {
		as.foldLeft(m.zero)((b, a) => m.op(b, f(a)))
	};System.out.println("""foldMapV2: [A, B](as: List[A], m: example02.Monoid[B])(f: A => B)B""");$skip(68); val res$11 = 

	foldMapV1[Int, String](1 :: 2 :: 3 :: Nil, stringMonoid)("" + _);System.out.println("""res11: String = """ + $show(res$11));$skip(68); val res$12 = 

	foldMapV2[Int, String](1 :: 2 :: 3 :: Nil, stringMonoid)("" + _);System.out.println("""res12: String = """ + $show(res$12));$skip(133); 

	def endoMonoid[A]: Monoid[A => A] = new Monoid[A => A] {
		def op(f: A => A, g: A => A) = f compose g
		val zero = (a: A) => a
	};System.out.println("""endoMonoid: [A]=> example02.Monoid[A => A]""");$skip(108); 

	def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B): B =
		foldMapV2(as, endoMonoid[B])(f.curried)(z);System.out.println("""foldRight: [A, B](as: List[A])(z: B)(f: (A, B) => B)B""");$skip(183); 

	// We can get the dual of any monoid just by flipping the `op`.
	def dual[A](m: Monoid[A]): Monoid[A] = new Monoid[A] {
		def op(x: A, y: A): A = m.op(y, x)
		val zero = m.zero
	};System.out.println("""dual: [A](m: example02.Monoid[A])example02.Monoid[A]""");$skip(329); 

	// Folding to the left is the same except we flip the arguments to
	// the function `f` to put the `B` on the correct side.
	// Then we have to also "flip" the monoid so that it operates from left to right.
	def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B): B =
		foldMapV2(as, dual(endoMonoid[B]))(a => b => f(b, a))(z);System.out.println("""foldLeft: [A, B](as: List[A])(z: B)(f: (B, A) => B)B""");$skip(243); 

	def foldMapV[A, B](as: IndexedSeq[A], m: Monoid[B])(f: A => B): B =
		if (as.length == 0)
			m.zero
		else if (as.length == 1)
			f(as(0))
		else {
			val (l, r) = as.splitAt(as.length / 2)
			m.op(foldMapV(l, m)(f), foldMapV(r, m)(f))
		};System.out.println("""foldMapV: [A, B](as: IndexedSeq[A], m: example02.Monoid[B])(f: A => B)B""")}
}
