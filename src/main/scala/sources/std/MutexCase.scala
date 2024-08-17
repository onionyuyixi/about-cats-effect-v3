package sources.std

import cats.effect.IO
import cats.effect.IO.race
import cats.effect.IOApp.Simple
import cats.effect.std.Mutex
import cats.implicits.catsSyntaxTuple2Parallel

import java.time.LocalDateTime
import scala.concurrent.duration.DurationInt

object MutexCase extends Simple{

  override def run: IO[Unit] = for {
    b <- Mutex[IO]
    _ <- IO.println(s"time is ${LocalDateTime.now()}")
    either <- b.lock.surround{
        IO.sleep(10.second) *>  IO.println("during mutex 1")
    } raceOutcome  b.lock.surround{
      IO.sleep(10.second) *>  IO.println("during mutex 2")
    }
    _ <- IO.println(s"the left result is ${either.left} the right result is ${either.right}")
    _ <- IO.println(s"time is ${LocalDateTime.now()}")
    _ <- IO.never[Int]
  }yield ()
}