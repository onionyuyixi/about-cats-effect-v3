package sources.cases

import cats.effect.kernel.Fiber
import cats.effect.{ExitCode, IOApp}

object GenSpawnCae extends App {

  import cats.effect.syntax.all._
  import cats.effect.{MonadCancel, Spawn}
  import cats.syntax.all._

  trait Server[F[_]] {
    def accept: F[Connection[F]]
  }

  trait Connection[F[_]] {
    def read: F[Array[Byte]]

    def write(bytes: Array[Byte]): F[Unit]

    def close: F[Unit]
  }

  def endpoint[F[_] : Spawn](server: Server[F])(body: Array[Byte] => F[Array[Byte]]): F[Unit] = {

    def handle(conn: Connection[F]): F[Unit] =
      for {
        request <- conn.read
        response <- body(request)
        _ <- conn.write(response)
      } yield ()

    val handler: F[Fiber[F, Throwable, Unit]] = MonadCancel[F] uncancelable { poll =>
      poll(server.accept) flatMap { conn =>
        handle(conn).guarantee(conn.close).start
      }
    }

    handler.foreverM
  }


}

object Spawn1 extends IOApp {


  import cats.effect.IO

  import scala.concurrent.duration._


  override def run(args: List[String]): IO[ExitCode] = for {
    target <- IO.println("Catch me if you can!").foreverM.start
    _ <- IO.sleep(1.second)
    _ <- target.cancel
  } yield ExitCode.Success
}

object Spawn2 extends IOApp {


  import cats.effect.IO
  import cats.syntax.all._


  override def run(args: List[String]): IO[ExitCode] = for {
    start <- IO.pure(System.currentTimeMillis())
    _ <- (1 to 1000*10000).toList.parTraverse(i => IO(5f / i))
    _ <- IO.println(s" par time used ${System.currentTimeMillis()-start}")
  } yield ExitCode.Success



}

object Spawn3 extends IOApp {


  import cats.effect.IO
  import cats.syntax.all._


  override def run(args: List[String]): IO[ExitCode] = for {
    start <- IO.pure(System.currentTimeMillis())
    _ <- (1 to 1000*10000).toList.traverse(i => IO(5f / i))
    _ <- IO.println(s" seq time used ${System.currentTimeMillis()-start}")
  } yield ExitCode.Success



}
object Spawn4 extends IOApp {


  import cats.effect.IO


  override def run(args: List[String]): IO[ExitCode] = for {
    start <- IO.apply(123/0).start
    _ <- start.cancel
    _ <- start.joinWithNever
  } yield ExitCode.Success



}




