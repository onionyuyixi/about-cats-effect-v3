package sources.cases

import cats.effect.{ExitCode, IO, IOApp}
import cats.implicits.catsSyntaxApplicativeByName

object UnderChecker extends IOApp{


  import scala.concurrent.duration._

  val starvationThreshold = 0.1.seconds
  val sleepInterval       = 1.second

  val tick: IO[Unit] =
    // grab a time offset from when we started
    IO.monotonic flatMap { start =>
      // sleep for some long interval, then measure when we woke up
      // get the difference (this will always be a bit more than the interval we slept for)
      IO.sleep(sleepInterval) *> IO.monotonic.map(_ - start) flatMap { delta =>
        // the delta here is the sum of the sleep interval and the amount of *lag* in the scheduler
        // specifically, the lag is the time between requesting CPU access and actually getting it
        IO.println("starvation detected").whenA(delta - sleepInterval > starvationThreshold)
      }
    }
  // add an initial delay
  // run the check infinitely, forked to a separate fiber
  val checker: IO[Unit] = (IO.sleep(1.second) *> tick.foreverM).start.void

  override def run(args: List[String]): IO[ExitCode] = for {
    _ <- checker
  }yield ExitCode.Success
}
