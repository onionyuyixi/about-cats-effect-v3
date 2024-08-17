package sources.cats_

import org.typelevel.scalaccompat.annotation.uncheckedVariance2

import scala.annotation.tailrec

sealed abstract class Eval[+A] extends Serializable {
  self =>

  def value: A

  def map[B](f: A => B): Eval[B] =
    flatMap(a => Now(f(a)))

  // continuous actions 都变成了FlatMap
  def flatMap[B](f: A => Eval[B]): Eval[B] =
    this match {
      case defer: Eval.Defer[A] =>
        new Eval.FlatMap[B] {
          type Start = A
          val start: () => Eval[Start] = defer.thunk
          val run: (A => Eval[B]) @uncheckedVariance2 = f
        }

      case c: Eval.FlatMap[A] =>
        new Eval.FlatMap[B] {
          type Start = c.Start
          val start: () => Eval[Start] = c.start
          val run: Start => Eval[B] = (s: c.Start) =>
            new Eval.FlatMap[B] {
              type Start = A
              val start: () => Eval[A] = () => c.run(s)
              val run: (A => Eval[B]) @uncheckedVariance2 = f
            }
        }


      case _ =>
        new Eval.FlatMap[B] {
          override type Start = A
          override val start: () => Eval[A] = () => self
          override val run: A => Eval[B] @uncheckedVariance2 = f
        }
    }

  def memoize: Eval[A]


}

object Eval {

  sealed abstract class Leaf[A] extends Eval[A]

  def now[A](a: A): Now[A] = Now(a)

  def later[A](a: => A): Later[A] = new Later[A](() => a)

  def always[A](a: => A): Always[A] = new Always(() => a)

  def defer[A](a: => Eval[A]): Defer[A] =
    new Defer[A](() => a) {}

  sealed abstract class Defer[A](val thunk: () => Eval[A]) extends Eval[A] {

    def value: A = evaluate(this)

    def memoize: Eval[A] = Memoize(this)

  }

  private sealed abstract class FlatMap[A] extends Eval[A] {
    self =>

    // 表示依赖上一个数据 即Start
    type Start

    // 一个起点
    val start: () => Eval[Start]

    // 一个函数
    val run: Start => Eval[A]

    // 需要递归计算
    def value: A = evaluate(this)

    def memoize: Eval[A] = Memoize(this)

  }

  // FlatMap可以改写成这样 FlatMap的语义 def flatMap (fa: =>F[A])(f:A=>F[B]):F[B]
  private sealed abstract class FlatMap1[A, B](start: () => Eval[A], run: A => Eval[B]) extends Eval[A]


  private case class Memoize[A](eval: Eval[A]) extends Eval[A] {

    var result: Option[A] = None

    def memoize: Eval[A] = this

    override def value: A =
      result match {
        case Some(value) => value
        case None =>
          val a = evaluate(this)
          result = Some(a)
          a
      }
  }


  sealed abstract private class FnStack[A, B]

  final private case class Ident[A, B](env: A <:< B) extends FnStack[A, B]

  //  类似flatMap的语义  rest 是个起点 first是个函数
  final private case class Many[A, B, C](first: A => Eval[B], rest: FnStack[B, C]) extends FnStack[A, C]


  private def evaluate[A](e: Eval[A]): A = {

    def addToMemo[A1](m: Memoize[A1]): A1 => Eval[A1] = {
      (a1: A1) =>
        m.result = Some(a1)
        Now(a1)
    }

    @tailrec
    def loop[A1](curr: Eval[A1], fs: FnStack[A1, A]): A = {
      curr match {

        case currentFlatMap: FlatMap[A1] =>

          val start: Eval[currentFlatMap.Start] = currentFlatMap.start()

          start match {

            case preDependFlatMap: FlatMap[currentFlatMap.Start] =>

              // 【先计算preDependFlatMap中的start 然后将接下来的数据currentFlatMap中的Start压入栈中,在关联上当前fs，构成新的nextFs】
              val start: () => Eval[preDependFlatMap.Start] = preDependFlatMap.start

              val run: currentFlatMap.Start => Eval[A1] = currentFlatMap.run

              val nextFs: Many[currentFlatMap.Start, A1, A] = Many(run, fs)

              val nextFs_ : Many[preDependFlatMap.Start, currentFlatMap.Start, A] = Many(preDependFlatMap.run, nextFs)

              loop(start(), nextFs_)

            case defer: Defer[currentFlatMap.Start] =>
              // 没有依赖 直接使用defer中的数据
              loop(defer.thunk(), Many(currentFlatMap.run, fs))

            case memoize@Memoize(eval) =>
              memoize.result match {
                case Some(value) =>
                  // memoize中已经存在数据(已经通过了计算) 则直接通过flatmap进行处理
                  val run: currentFlatMap.Start => Eval[A1] = currentFlatMap.run
                  loop(run(value), fs)
                case None =>
                  // memoize中没有数据 则直接采用构造器中的Eval数据
                  val nextFs = Many(currentFlatMap.run, fs)
                  loop(eval, nextFs)
              }

            case leaf: Leaf[currentFlatMap.Start] =>
              loop(currentFlatMap.run(leaf.value), fs)
          }

        case defer: Defer[A1] =>
          loop(defer.thunk(), fs)

        case m: Memoize[a] =>
          m.result match {
            case Some(value) =>
              fs match {
                case Ident(env) => env(value)
                case Many(first, rest) => loop(first(value), rest)
              }
            case None =>
              loop[a](m.eval, Many[a, A1, A](addToMemo[a](m), fs))
          }

        case leaf: Leaf[A1] =>
          val a1 = leaf.value
          fs match {
            case Ident(env) => env(a1)
            case Many(first, rest) => loop(first(a1), rest)
          }
      }

    }

    loop(e, Ident(implicitly[A <:< A]))


  }


}

final case class Now[A](value: A) extends Eval.Leaf[A] {

  def memoize: Eval[A] = this

}


final class Later[A](f: () => A) extends Eval.Leaf[A] {

  def memoize: Eval[A] = this

  private var thunk: () => A = f


  lazy val value: A = {
    val result = thunk()
    thunk = null
    result
  }

}

object Later {
  def apply[A](a: => A): Later[A] = new Later(() => a)
}

final class Always[A](f: () => A) extends Eval.Leaf[A] {

  override def value: A = f()

  // 每次重新计算 生成一个新的对象
  def memoize: Eval[A] = new Later(f)
}

object Always {

  def apply[A](a: => A): Always[A] = new Always[A](() => a)

}





