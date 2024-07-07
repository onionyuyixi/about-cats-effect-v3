package sources.std

import cats.effect.{ExitCode, IO, IOApp}
import cats.effect.std.CyclicBarrier
import cats.syntax.all._
import scala.concurrent.duration.DurationInt

object CycleBarrierCase extends IOApp {

  override def run(args: List[String]): IO[ExitCode] = for {
    cycleBarrier <- CyclicBarrier[IO](1)
    f1 <- (IO.println("fast fiber before barrier") >>
      cycleBarrier.await >>
      IO.println("fast fiber after barrier")).start

    f2 <- (
      IO.sleep(2.second) >>
      IO.println("slow fiber before barrier") >>
      cycleBarrier.await >>
      IO.println("slow fiber after barrier")
      ).start
    _ <- (f1.join,f2.join).tupled
  } yield ExitCode.Success


}
