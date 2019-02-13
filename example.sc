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

	SeqF.map(List(1, 2, 3, 4))(fii)           //> res1: Seq[Int] = List(2, 4, 6, 8)
	SeqF.map(List.empty[Int])(fii)            //> res2: Seq[Int] = List()
	OptionF.map(Some(2))(fii)                 //> res3: Option[Int] = Some(4)
	OptionF.map(Option.empty[Int])(fii)       //> res4: Option[Int] = None

	val fa = FunctionF.map(fid)(fds)          //> fa  : Int => String = example$FunctionF$2$$anon$1$$Lambda$17/1307096070@3c7
                                                  //| 56e4d
	val fb = FunctionF.map[Int, Double, String](fid)(fds)
                                                  //> fb  : Int => String = example$FunctionF$2$$anon$1$$Lambda$17/1307096070@7c0
                                                  //| e2abd
	val fc = fds compose fid                  //> fc  : Int => String = scala.Function1$$Lambda$18/1223685984@402f32ff
	fa(2)                                     //> res5: String = 4.2
	fb(2)                                     //> res6: String = 4.2
	fc(2)                                     //> res7: String = 4.2

	val arr = Array(1, 2, 3, 3, 3)            //> arr  : Array[Int] = Array(1, 2, 3, 3, 3)
	arr.groupBy(identity)                     //> res8: scala.collection.immutable.Map[Int,Array[Int]] = Map(2 -> Array(2), 1
                                                  //|  -> Array(1), 3 -> Array(3, 3, 3))
                                                  
                                                  
                                                  
                                                  
}