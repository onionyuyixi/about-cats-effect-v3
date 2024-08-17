package sources.cases

import cats.Applicative
import cats.data.Nested
import cats.implicits._

import java.sql.Connection
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

object ApplicativeCase extends App {


  val x: Future[Option[Int]] = Future.successful(Some(5))
  val y: Future[Option[Char]] = Future.successful(Some('a'))

  val composed = Applicative[Future].compose[Option].map2(x, y)((x, y) => (x, y))

  println(composed)

  val nested = Applicative[({type l[X] = Nested[Future, Option, X]})#l].map2(Nested(x), Nested(y))(_ + _)

  println(nested.value.value)

  def traverseOption[A, B](as: List[A])(f: A => Option[B]): Option[List[B]] =
    as.foldRight(Some(List.empty[B]): Option[List[B]]) { (a, acc) =>
      Applicative[Option].map2(f(a): Option[B], acc: Option[List[B]])(_ :: _)
    }

  println(traverseOption(List(1, 2, 3, 4))(a => Some(a + 1)))

  println(List(1, 2, 3, 4).traverse(a => Some(a + 1): Option[Int]))


  val username: Option[String] = Some("username")
  val password: Option[String] = Some("password")
  val url: Option[String] = Some("some.login.url.here")

  def attemptConnect(username: String, password: String, url: String): Option[Connection] = None

  //Applicative[Option].map3(username, password, url)(attemptConnect) 的简写
  (username, password, url) mapN (attemptConnect)

  private val tupled: Option[(String, String)] = (username, password).tupled




}
