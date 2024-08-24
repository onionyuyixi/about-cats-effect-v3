package sources.cases

import cats.effect.kernel.Outcome.Succeeded
import cats.effect.std.Supervisor
import cats.effect.{IO, IOApp, Outcome}
import cats.implicits._

import scala.concurrent.duration.DurationInt

object SupervisorCase extends IOApp.Simple {

  val ioa: IO[String] = IO.sleep(1.seconds) >> IO(s"ioa ${Thread.currentThread().getName}")

  override def run: IO[Unit] = {
    val io: IO[Unit] = Supervisor[IO].use {
      supervisor => {
        // 给ioa生成的新fiber 绑定一个指定的生命周期
        val iob = supervisor.supervise(ioa).flatMap(f => {
          f.join.flatMap {
            case Succeeded(fa) => fa.map(a => a + s" in supervisor ${Thread.currentThread().getName}")
            case Outcome.Errored(e) => IO(e.getMessage)
            case Outcome.Canceled() => IO("取消")
          }
        })

        (iob.flatMap((b: String) => IO.println(s"over!!! the result is ioa-->$b ")) *>
          IO.sleep(1000.milliseconds).start *>
          supervisor.supervise(IO.println(s"after 1s ${}"))
          ).void
      }
    }

    (ioa -> io).flatMapN((str, _) => IO.println(s"ioa result is $str")).void
  }
}
