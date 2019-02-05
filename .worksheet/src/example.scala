import jadeutils.common.StrUtils

object example {;import org.scalaide.worksheet.runtime.library.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(95); 
  println("Welcome to the Scala worksheet");$skip(74); 
  val aa = java.util.Base64.getEncoder.encode("hello扫地 world".getBytes());System.out.println("""aa  : Array[Byte] = """ + $show(aa ));$skip(27); 
  val str = new String(aa);System.out.println("""str  : String = """ + $show(str ));$skip(62); 
  var ab = java.util.Base64.getDecoder.decode(str.getBytes());System.out.println("""ab  : Array[Byte] = """ + $show(ab ));$skip(28); 
  val str2 = new String(ab);System.out.println("""str2  : String = """ + $show(str2 ));$skip(54); 

  val str3 = StrUtils.encodeBase64("hello扫地 world");System.out.println("""str3  : String = """ + $show(str3 ));$skip(40); 
  val str5 = StrUtils.decodeBase64(str)

  import scala.language.higherKinds

  /**
   * 半群（SemiGroup）是一组对象的集合，满应足封闭性和结合性。
   * A表示群的构成对象，op表示两个对象的结合，它的封闭性由抽象类型A保证。
   */
  trait SemiGroup[A] {
    def op(a1: A, a2: A): A
  }

  /**
   * 幺半群（Monoid）是半群（SemiGroup）的子集，并且存在一个幺元。
   */
  trait Monoid[A] extends SemiGroup[A] {
    def zero: A
  };System.out.println("""str5  : String = """ + $show(str5 ));$skip(460); 

  /**
   * String的幺半群实现，它的幺元是空字符串
   */
  val stringMonoid = new Monoid[String] {
    def op(a1: String, a2: String) = a1 + a2

    def zero = ""
  };System.out.println("""stringMonoid  : example.Monoid[String] = """ + $show(stringMonoid ));$skip(142); 

  /**
   * 整数加法的的幺半群实现，它的幺元是0
   */
  val intAdditionMonoid = new Monoid[Int] {
    def op(a1: Int, a2: Int) = a1 + a2
    def zero = 0
  };System.out.println("""intAdditionMonoid  : example.Monoid[Int] = """ + $show(intAdditionMonoid ));$skip(237); 

  /**
   * 整数加法的的幺半群实现，它的幺元是0
   * 对于不同的幺半群群，它们的结合行为，和幺元是不一样的。
   * 当自己实现一个群时一定要注意这点。
   * 比如对于Int的幺半群，在加法和乘法的情况下幺元分别是0和1。
   */
  val intMultiplyMonoid = new Monoid[Int] {
    def op(a1: Int, a2: Int) = a1 * a2

    def zero = 1
  };System.out.println("""intMultiplyMonoid  : example.Monoid[Int] = """ + $show(intMultiplyMonoid ));$skip(154); 

  /**
   * 列表的幺半群实现，它的幺元是空列表
   */
  def listMonoid[A] = new Monoid[List[A]] {
    def op(a1: List[A], a2: List[A]) = a1 ++ a2

    def zero = Nil
  };System.out.println("""listMonoid: [A]=> example.Monoid[List[A]]{def zero: scala.collection.immutable.Nil.type}""");$skip(169); 

  /**
   * Option的幺半群实现，它的幺元是空
   */
  def optionMonoid[A] = new Monoid[Option[A]] {
    def op(a1: Option[A], a2: Option[A]) = a1 orElse a2

    def zero = None
  }

  /**
   * 函子的抽像特质
   * `Functor`的作用是为容器中每个成员都映射出一个新的成员来：
   *
   * 范型定义为`F[_]`，表示它可以应用到成员为任何类型的容器。
   * `A`表示原来的类型，`B`表示新的类型。
   * `map`方法的参数`fa: F[A]`表示内容为`A`类型的源容器。
   * `map`方法的参数`f: A => B`表示把成员从`A`类型转为`B`类型的方法。
   * `map`方法的返回类型` F[B]`表示返回值是内容为`B`类型的目标容器。
   */
  trait Functor[F[_]] {
    def map[A, B](fa: F[A])(f: A => B): F[B]
  }

  /**
   * `SeqF`实现了函子应用到容器类型为`Seq`的实现：
   */
  object SeqF extends Functor[Seq] {
    def map[A, B](seq: Seq[A])(f: A => B): Seq[B] = seq map f
  }

  /**
   * `OptionF`实现了函子应用到容器类型为`Option`的实现：
   */
  object OptionF extends Functor[Option] {
    def map[A, B](opt: Option[A])(f: A => B): Option[B] = opt map f
  }

  /**
   * `FunctionF`实现了函子应用到容器类型为`Function`的实现，
   * 把`A -> C`和`C -> B`两个函数转为一个新的函数`A -> B`：
   */
  object FunctionF {
    def map[A, C, B](func: A => C)(f: C => B): A => B = {
      val functor = new Functor[({ type L[D] = A => D })#L] {
        def map[E, B](func: A => E)(f: E => B): A => B =
          (a: A) => f(func(a))
      }
      functor.map(func)(f)
    }
  };System.out.println("""optionMonoid: [A]=> example.Monoid[Option[A]]{def zero: None.type}""");$skip(1077); 

  val fii: Int => Int = i => 2 * i;System.out.println("""fii  : Int => Int = """ + $show(fii ));$skip(40); 
  val fid: Int => Double = i => 2.1 * i;System.out.println("""fid  : Int => Double = """ + $show(fid ));$skip(46); 
  val fds: Double => String = d => d.toString;System.out.println("""fds  : Double => String = """ + $show(fds ));$skip(36); val res$0 = 

  SeqF.map(List(1, 2, 3, 4))(fii);System.out.println("""res0: Seq[Int] = """ + $show(res$0));$skip(33); val res$1 = 
  SeqF.map(List.empty[Int])(fii);System.out.println("""res1: Seq[Int] = """ + $show(res$1));$skip(28); val res$2 = 
  OptionF.map(Some(2))(fii);System.out.println("""res2: Option[Int] = """ + $show(res$2));$skip(38); val res$3 = 
  OptionF.map(Option.empty[Int])(fii);System.out.println("""res3: Option[Int] = """ + $show(res$3));$skip(37); 

  val fa = FunctionF.map(fid)(fds);System.out.println("""fa  : Int => String = """ + $show(fa ));$skip(56); 
  val fb = FunctionF.map[Int, Double, String](fid)(fds);System.out.println("""fb  : Int => String = """ + $show(fb ));$skip(27); 
  val fc = fds compose fid;System.out.println("""fc  : Int => String = """ + $show(fc ));$skip(8); val res$4 = 
  fa(2);System.out.println("""res4: String = """ + $show(res$4));$skip(8); val res$5 = 
  fb(2);System.out.println("""res5: String = """ + $show(res$5));$skip(8); val res$6 = 
  fc(2)

  /**
   * Monad就是是一个函子：`M: C -> C`。
   * 并且对`C`中的每一个对象`x`有以下两个射态：
   * 1. `unit: x -> M[X]`
   * 2. `join/bind: M[M[x]] -> M[x]`
   */
  trait MonadTrait[M[_]] {
    def unit[A](a: A): M[A] //identity
    def join[A](mma: M[M[A]]): M[A]
  };System.out.println("""res6: String = """ + $show(res$6));$skip(264); 

  val s = Some(1);System.out.println("""s  : Some[Int] = """ + $show(s ));$skip(35); 
  val sm = s.map(i => Some(i + 1));System.out.println("""sm  : Option[Some[Int]] = """ + $show(sm ));$skip(44); 
  val smf = s.map(i => Some(i + 1)).flatten;System.out.println("""smf  : Option[Int] = """ + $show(smf ));$skip(39); 
  val sf = s.flatMap(i => Some(i + 1));System.out.println("""sf  : Option[Int] = """ + $show(sf ));$skip(81); 
  val isFmEqual = s.map(i => Some(i + 1)).flatten == s.flatMap(i => Some(i + 1));System.out.println("""isFmEqual  : Boolean = """ + $show(isFmEqual ))}

  trait Monad[M[_]] {
    def unit[A](a: A): M[A]
    def flatMap[A, B](fa: M[A])(f: A => M[B]): M[B]
  }

}
