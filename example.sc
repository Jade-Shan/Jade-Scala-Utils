import jadeutils.common.StrUtils

object example {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet
  val aa = java.util.Base64.getEncoder.encode("hello扫地 world".getBytes())
                                                  //> aa  : Array[Byte] = Array(97, 71, 86, 115, 98, 71, 47, 109, 105, 97, 118, 10
                                                  //| 8, 110, 76, 65, 103, 100, 50, 57, 121, 98, 71, 81, 61)
  val str = new String(aa)                        //> str  : String = aGVsbG/miavlnLAgd29ybGQ=
  var ab = java.util.Base64.getDecoder.decode(str.getBytes())
                                                  //> ab  : Array[Byte] = Array(104, 101, 108, 108, 111, -26, -119, -85, -27, -100
                                                  //| , -80, 32, 119, 111, 114, 108, 100)
  val str2 = new String(ab)                       //> str2  : String = hello扫地 world

  val str3 = StrUtils.encodeBase64("hello扫地 world")
                                                  //> str3  : String = aGVsbG/miavlnLAgd29ybGQ=
  val str5 = StrUtils.decodeBase64(str)           //> str5  : String = hello扫地 world

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
  }

  /**
   * String的幺半群实现，它的幺元是空字符串
   */
  val stringMonoid = new Monoid[String] {
    def op(a1: String, a2: String) = a1 + a2

    def zero = ""
  }                                               //> stringMonoid  : example.Monoid[String] = example$$anon$1@1936f0f5

  /**
   * 整数加法的的幺半群实现，它的幺元是0
   */
  val intAdditionMonoid = new Monoid[Int] {
    def op(a1: Int, a2: Int) = a1 + a2
    def zero = 0
  }                                               //> intAdditionMonoid  : example.Monoid[Int] = example$$anon$2@6615435c

  /**
   * 整数加法的的幺半群实现，它的幺元是0
   * 对于不同的幺半群群，它们的结合行为，和幺元是不一样的。
   * 当自己实现一个群时一定要注意这点。
   * 比如对于Int的幺半群，在加法和乘法的情况下幺元分别是0和1。
   */
  val intMultiplyMonoid = new Monoid[Int] {
    def op(a1: Int, a2: Int) = a1 * a2

    def zero = 1
  }                                               //> intMultiplyMonoid  : example.Monoid[Int] = example$$anon$3@4909b8da

  /**
   * 列表的幺半群实现，它的幺元是空列表
   */
  def listMonoid[A] = new Monoid[List[A]] {
    def op(a1: List[A], a2: List[A]) = a1 ++ a2

    def zero = Nil
  }                                               //> listMonoid: [A]=> example.Monoid[List[A]]{def zero: scala.collection.immuta
                                                  //| ble.Nil.type}

  /**
   * Option的幺半群实现，它的幺元是空
   */
  def optionMonoid[A] = new Monoid[Option[A]] {
    def op(a1: Option[A], a2: Option[A]) = a1 orElse a2

    def zero = None
  }                                               //> optionMonoid: [A]=> example.Monoid[Option[A]]{def zero: None.type}

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
  }

  val fii: Int => Int = i => 2 * i                //> fii  : Int => Int = example$$$Lambda$12/1740035246@34b7bfc0
  val fid: Int => Double = i => 2.1 * i           //> fid  : Int => Double = example$$$Lambda$13/1915058446@54a097cc
  val fds: Double => String = d => d.toString     //> fds  : Double => String = example$$$Lambda$14/922151033@5a61f5df

  SeqF.map(List(1, 2, 3, 4))(fii)                 //> res0: Seq[Int] = List(2, 4, 6, 8)
  SeqF.map(List.empty[Int])(fii)                  //> res1: Seq[Int] = List()
  OptionF.map(Some(2))(fii)                       //> res2: Option[Int] = Some(4)
  OptionF.map(Option.empty[Int])(fii)             //> res3: Option[Int] = None

  val fa = FunctionF.map(fid)(fds)                //> fa  : Int => String = example$FunctionF$2$$anon$6$$Lambda$17/1166726978@5af
                                                  //| a04c
  val fb = FunctionF.map[Int, Double, String](fid)(fds)
                                                  //> fb  : Int => String = example$FunctionF$2$$anon$6$$Lambda$17/1166726978@6ea
                                                  //| 12c19
  val fc = fds compose fid                        //> fc  : Int => String = scala.Function1$$Lambda$18/1778535015@7921b0a2
  fa(2)                                           //> res4: String = 4.2
  fb(2)                                           //> res5: String = 4.2
  fc(2)                                           //> res6: String = 4.2

  /**
   * Monad就是是一个函子：`M: C -> C`。
   * 并且对`C`中的每一个对象`x`有以下两个射态：
   * 1. `unit: x -> M[X]`
   * 2. `join/bind: M[M[x]] -> M[x]`
   */
  trait MonadTrait[M[_]] {
    def unit[A](a: A): M[A] //identity
    def join[A](mma: M[M[A]]): M[A]
  }

  val s = Some(1)                                 //> s  : Some[Int] = Some(1)
  val sm = s.map(i => Some(i + 1))                //> sm  : Option[Some[Int]] = Some(Some(2))
  val smf = s.map(i => Some(i + 1)).flatten       //> smf  : Option[Int] = Some(2)
  val sf = s.flatMap(i => Some(i + 1))            //> sf  : Option[Int] = Some(2)
  val isFmEqual = s.map(i => Some(i + 1)).flatten == s.flatMap(i => Some(i + 1))
                                                  //> isFmEqual  : Boolean = true

  trait Monad[M[_]] {
    def unit[A](a: A): M[A]
    def flatMap[A, B](fa: M[A])(f: A => M[B]): M[B]
  }

}