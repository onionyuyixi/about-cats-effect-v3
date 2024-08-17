package sources.cases

import cats.{Applicative, Eval, Foldable, Semigroupal}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.DurationInt
import scala.concurrent.{Await, Future}

object FoldCase extends App {


  private val ints: List[Int] = (1 to 1 * 1000).to(List)

  private val l1: Long = System.currentTimeMillis()
  (1 to 10000) foreach { _ =>
    ints.foldLeft(0L)(_ + _)
  }
  println(System.currentTimeMillis() - l1)

  private val l4: Long = System.currentTimeMillis()
  (1 to 10000) foreach { _ =>
    ints.foldRight(0L)(_ + _)
  }
  println(System.currentTimeMillis() - l4)

  private val l2: Long = System.currentTimeMillis()

  (1 to 10000) foreach { _ =>
    val value: Eval[Long] = Foldable[List].foldRight(ints, Eval.now(0L)) { (num, eval) =>
      eval.map(_ + num)
    }
    value.value
  }
  println(System.currentTimeMillis() - l2)

  private val l3: Long = System.currentTimeMillis()

  (1 to 10000) foreach { _ =>
    val value: Long = Foldable[List].foldLeft(ints, 0L) { (result, num) =>
      result + num
    }
  }
  println(System.currentTimeMillis() - l3)


  import cats.syntax.all._

  val hostnames = List(
    "alpha.example.com",
    "beta.example.com",
    "gamma.demo.com"
  )

  def getUptime(hostname: String): Future[Int] =
    Future(hostname.length * 60)

  val allUptimes: Future[List[Int]] =
    hostnames.foldLeft(Future(List.empty[Int])) {
      (accum, host) =>
        val uptime: Future[Int] = getUptime(host)
        for {
          accum <- accum
          uptime <- uptime
        } yield accum :+ uptime
    }

  private val ints1: List[Int] = Await.result(allUptimes, 1.second)

  val allUptimes1: Future[List[Int]] = Future.traverse(hostnames)(getUptime)
  private val ints2: List[Int] = Await.result(allUptimes1, 1.second)

  println(ints1)
  println(ints2)


  def listTraverse[F[_] : Applicative, A, B](list: List[A])(func: A => F[B]): F[List[B]] =
    list.foldLeft(List.empty[B].pure[F]) { (accum, item) =>
      val value: F[B] = func(item)
      val value1: F[List[B]] = (accum, value).mapN(_ :+ _)
      Semigroupal.map2(accum, value)((a, b) => {
        val bs: List[B] = a :+ b
        println(s" bs  $bs")
        bs
      })
      println(s"after mapn $value1")
      value1
    }

  def listSequence[F[_] : Applicative, B](list: List[F[B]]): F[List[B]] =
    listTraverse(list)(identity)

  listSequence(List(Vector(1, 2), Vector(3, 4)))

  listSequence(List(Vector(1, 2), Vector(3, 4), Vector(5, 6)))

  listSequence(List(List(1, 2), List(3, 4), List(5, 6)))


  println(Vector(1, 2).map(a => " i am " + a))
  println(Vector(1, 2).toList.map(a=>"i am " + a))

}
