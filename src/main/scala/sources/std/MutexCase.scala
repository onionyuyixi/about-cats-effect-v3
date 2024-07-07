package sources.std

import cats.effect.{IO, Sync}
import cats.effect.IOApp.Simple
import cats.effect.kernel.Ref
import cats.effect.std.Mutex

import java.time.LocalDateTime
import scala.concurrent.duration.DurationInt

object MutexCase extends Simple{

  override def run: IO[Unit] = for {
    b <- Mutex[IO]
    _ <- IO.println(s"time is ${LocalDateTime.now()}")
    _ <- b.lock.surround{
        IO.sleep(10.second) *>  IO.println("during mutex")
    }
    _ <- IO.println(s"time is ${LocalDateTime.now()}")
  }yield ()
}