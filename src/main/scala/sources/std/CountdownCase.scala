package sources.std

import cats.effect.IO
import cats.effect.IOApp.Simple
import cats.effect.std.CountDownLatch

object CountdownCase extends Simple {

  override def run: IO[Unit] = for {
    c <- CountDownLatch[IO](2)
    f <- (c.await >> IO.println("countdown latch unblock")).start
    _ <- c.release
    _ <- IO.println("before latch is unblocked")
    _ <- c.release
    _ <- f.join
    _ <- c.await >> IO.println("await again")
  } yield ()
}
