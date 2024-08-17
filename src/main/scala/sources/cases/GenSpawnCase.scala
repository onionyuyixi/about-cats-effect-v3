package sources.cases

import cats.effect.kernel.Fiber
import cats.effect.{ExitCode, IOApp, Spawn}

import scala.concurrent.duration.DurationInt

object GenSpawnCase extends App {

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
    _ <- (1 to 1000 * 10000).toList.parTraverse(i => IO(5f / i))
    _ <- IO.println(s" par time used ${System.currentTimeMillis() - start}")
  } yield ExitCode.Success


}

object Spawn3 extends IOApp {


  import cats.effect.IO
  import cats.syntax.all._


  override def run(args: List[String]): IO[ExitCode] = for {
    start <- IO.pure(System.currentTimeMillis())
    _ <- (1 to 1000 * 10000).toList.traverse(i => IO(5f / i))
    _ <- IO.println(s" seq time used ${System.currentTimeMillis() - start}")
  } yield ExitCode.Success


}

object Spawn4 extends IOApp {


  import cats.effect.IO


  override def run(args: List[String]): IO[ExitCode] = for {
    start <- IO.apply(123 / 0).start
    _ <- start.cancel
    _ <- start.joinWithNever
  } yield ExitCode.Success


}

object Spawn5 extends IOApp {


  import cats.effect.IO


  override def run(args: List[String]): IO[ExitCode] = for {

    fiber <- IO.delay(println("A")).foreverM.start.debug()

    fiber2 <- IO.sleep(10.seconds).start

    _ <- fiber2.cancel

    oc <- fiber2.join

    _ <- IO.println(s"outcome is $oc")

  } yield ExitCode.Success


}


object Spawn6 extends IOApp {


  import cats.effect.IO


  override def run(args: List[String]): IO[ExitCode] = for {
    _ <- IO.println("cancelable demo")
    value = IO.println("不可取消").onCancel(IO.println("已经被取消"))
    _ <- implicitly[Spawn[IO]]
      .cancelable(IO.uncancelable(_ => value), IO.unit)
  } yield ExitCode.Success


}


object Spawn7 extends IOApp {


  import cats.effect.IO


  override def run(args: List[String]): IO[ExitCode] = for {
    _ <- IO.println("race demo")
    io = IO.sleep(10.seconds).onCancel(IO.println("i was canceled")) *> IO.println("after 10s")
    io1 = IO.sleep(10.seconds).onCancel(IO.println("被取消"))
    f <- io.start
    f1 <- io1.start
    _ <- IO.sleep(2.seconds)
    _ <- f.cancel
    _ <- f1.cancel
    race <- IO.race(io, io1)
    _ <- IO.println(s"race result $race")
    _ <- IO.println((8 - 1) >> 3)
    _ <- IO.println((3) >> 3)
  } yield ExitCode.Success


}


//object Spawn8 extends IOApp {
//
//
//  import cats.effect.IO
//
//
//  override def run(args: List[String]): IO[ExitCode] = {
//    val loop = IO.println("Hello, World!") >> loop
//    loop.timeout(5.seconds)
//    IO(ExitCode.Success)
//  }
//
//
//}



