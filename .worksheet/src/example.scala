import jadeutils.common.StrUtils

import scala.language.higherKinds
import java.text.SimpleDateFormat
import java.util.TimeZone

object example {;import org.scalaide.worksheet.runtime.library.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(193); 
	val sdf = new SimpleDateFormat("yyyy-MM-dd");System.out.println("""sdf  : java.text.SimpleDateFormat = """ + $show(sdf ));$skip(57); 
	sdf.setTimeZone(TimeZone.getTimeZone("Asia/Shanghai"));$skip(41); ;
	val startTime = sdf.parse("2019-01-28");System.out.println("""startTime  : java.util.Date = """ + $show(startTime ));$skip(22); val res$0 = 
	startTime.getTime();System.out.println("""res0: Long = """ + $show(res$0));$skip(45); ;

	println("Welcome to the Scala worksheet");$skip(73); 
	val aa = java.util.Base64.getEncoder.encode("hello扫地 world".getBytes());System.out.println("""aa  : Array[Byte] = """ + $show(aa ));$skip(26); 
	val str = new String(aa);System.out.println("""str  : String = """ + $show(str ));$skip(61); 
	var ab = java.util.Base64.getDecoder.decode(str.getBytes());System.out.println("""ab  : Array[Byte] = """ + $show(ab ));$skip(27); 
	val str2 = new String(ab);System.out.println("""str2  : String = """ + $show(str2 ));$skip(53); 

	val str3 = StrUtils.encodeBase64("hello扫地 world");System.out.println("""str3  : String = """ + $show(str3 ));$skip(39); 
	val str5 = StrUtils.decodeBase64(str)

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
	};System.out.println("""str5  : String = """ + $show(str5 ));$skip(573); 

	val fii: Int => Int = i => 2 * i;System.out.println("""fii  : Int => Int = """ + $show(fii ));$skip(39); 
	val fid: Int => Double = i => 2.1 * i;System.out.println("""fid  : Int => Double = """ + $show(fid ));$skip(45); 
	val fds: Double => String = d => d.toString;System.out.println("""fds  : Double => String = """ + $show(fds ));$skip(35); val res$1 = 

	SeqF.map(List(1, 2, 3, 4))(fii);System.out.println("""res1: Seq[Int] = """ + $show(res$1));$skip(32); val res$2 = 
	SeqF.map(List.empty[Int])(fii);System.out.println("""res2: Seq[Int] = """ + $show(res$2));$skip(27); val res$3 = 
	OptionF.map(Some(2))(fii);System.out.println("""res3: Option[Int] = """ + $show(res$3));$skip(37); val res$4 = 
	OptionF.map(Option.empty[Int])(fii);System.out.println("""res4: Option[Int] = """ + $show(res$4));$skip(36); 

	val fa = FunctionF.map(fid)(fds);System.out.println("""fa  : Int => String = """ + $show(fa ));$skip(55); 
	val fb = FunctionF.map[Int, Double, String](fid)(fds);System.out.println("""fb  : Int => String = """ + $show(fb ));$skip(26); 
	val fc = fds compose fid;System.out.println("""fc  : Int => String = """ + $show(fc ));$skip(7); val res$5 = 
	fa(2);System.out.println("""res5: String = """ + $show(res$5));$skip(7); val res$6 = 
	fb(2);System.out.println("""res6: String = """ + $show(res$6));$skip(7); val res$7 = 
	fc(2);System.out.println("""res7: String = """ + $show(res$7));$skip(34); 

	val arr = Array(1, 2, 3, 3, 3);System.out.println("""arr  : Array[Int] = """ + $show(arr ));$skip(23); val res$8 = 
	arr.groupBy(identity);System.out.println("""res8: scala.collection.immutable.Map[Int,Array[Int]] = """ + $show(res$8))}
}
