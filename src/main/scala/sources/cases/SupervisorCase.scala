package sources.cases

import cats.effect
import cats.effect.kernel.Outcome
import cats.effect.{IO, Resource}
import cats.effect.std.Supervisor
import cats.effect.unsafe.implicits.global

object SupervisorCase extends App {


  val program: Resource[IO, IO[effect.FiberIO[Unit]]] = for {
    s <- Supervisor[IO]
  } yield {
    s.supervise(IO.println(s"this is a fiber ${Thread.currentThread().getName}"))
      .flatMap(fiber => {
        fiber.join.map {
          case Outcome.Succeeded(fa) => println(s"result is $fa")
          case Outcome.Errored(e) => println(s"error is $e")
          case Outcome.Canceled() => println("canceled")
        }
      }).start
  }

  program.use(_ => IO.println(s"mmmm ${Thread.currentThread().getName}"))
    .unsafeRunSync()

}
