import jadeutils.common.StrUtils

import scala.language.higherKinds
import java.text.SimpleDateFormat
import java.util.TimeZone

object example {
	val sdf = new SimpleDateFormat("yyyy-MM-dd")
                                                  //> sdf  : java.text.SimpleDateFormat = java.text.SimpleDateFormat@f67a0200
	sdf.setTimeZone(TimeZone.getTimeZone("Asia/Shanghai"));
	val startTime = sdf.parse("2019-01-28")   //> startTime  : java.util.Date = Mon Jan 28 00:00:00 CST 2019
	startTime.getTime();                      //> res0: Long = 1548604800000

	println("Welcome to the Scala worksheet") //> Welcome to the Scala worksheet
	val aa = java.util.Base64.getEncoder.encode("hello扫地 world".getBytes())
                                                  //> aa  : Array[Byte] = Array(97, 71, 86, 115, 98, 71, 47, 109, 105, 97, 118, 10
                                                  //| 8, 110, 76, 65, 103, 100, 50, 57, 121, 98, 71, 81, 61)
	val str = new String(aa)                  //> str  : String = aGVsbG/miavlnLAgd29ybGQ=
	var ab = java.util.Base64.getDecoder.decode(str.getBytes())
                                                  //> ab  : Array[Byte] = Array(104, 101, 108, 108, 111, -26, -119, -85, -27, -100
                                                  //| , -80, 32, 119, 111, 114, 108, 100)
	val str2 = new String(ab)                 //> str2  : String = hello扫地 world

	val str3 = StrUtils.encodeBase64("hello扫地 world")
                                                  //> str3  : String = aGVsbG/miavlnLAgd29ybGQ=
	val str5 = StrUtils.decodeBase64(str)     //> str5  : String = hello扫地 world

	trait Functor[F[_]] {
		def map[A, B](fa: F[A])(f: A => B): F[B]
	}

	object SeqF extends Functor[Seq] {
		def map[A, B](seq: Seq[A])(f: A => B): Seq[B] = seq map f
	}

	object OptionF extends Functor[Option] {
		def map[A, B](opt: Option[A])(f: A => B): Option[B] = opt map f
	}

	object FunctionF {
		def map[A, C, B](func: A => C)(f: C => B): A => B = {
			val functor = new Functor[({ type L[D] = A => D })#L] {
				def map[E, B](func: A => E)(f: E => B): A => B =
					(a: A) => f(func(a))
			}
			functor.map(func)(f)
		}
	}

	val fii: Int => Int = i => 2 * i          //> fii  : Int => Int = example$$$Lambda$12/1516369375@3551a94
	val fid: Int => Double = i => 2.1 * i     //> fid  : Int => Double = example$$$Lambda$13/546718765@9f70c54
	val fds: Double => String = d => d.toString
                                                  //> fds  : Double => String = example$$$Lambda$14/592179046@737996a0

	SeqF.map(List(1, 2, 3, 4))(fii)
	SeqF.map(List.empty[Int])(fii)
	OptionF.map(Some(2))(fii)
	OptionF.map(Option.empty[Int])(fii)

	val fa = FunctionF.map(fid)(fds)
	val fb = FunctionF.map[Int, Double, String](fid)(fds)
	val fc = fds compose fid
	fa(2)
	fb(2)
	fc(2)

	val arr = Array(1, 2, 3, 3, 3)
	arr.groupBy(identity)
}