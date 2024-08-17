package sources.cases

import cats.effect.std.Dequeue
import cats.effect.{IO, IOApp}
import cats.implicits._

import java.time.LocalDateTime
import scala.concurrent.duration.DurationInt

object DequeueCase extends IOApp.Simple {

  override def run: IO[Unit] = for {
    q <- Dequeue.bounded[IO, Int](10)
    _ <- q.offer(1).debug("添加数据-1")
    _ <- q.offer(2).debug("添加数据-2")
    f1 <- q.offer(3).debug("添加数据-3").start
    f2 <- q.offer(4).debug("添加数据-4").start
    _ <- f1.join
    _ <- f2.join
    _ <- addData()
    _ <- q.take.debug("remove").start
    _ <- q.take.debug("remove").start
    _ <- q.take.debug("remove").start
    _ <- q.take.debug("remove").start
  } yield println("11111111111111")


  def addData(): IO[Unit] = {
    val ioQueue: IO[Dequeue[IO, Int]] = Dequeue.bounded[IO, Int](18)
    IO.pure(Console.println(s"start time is ${LocalDateTime.now()}")) >>
      ioQueue.flatMap { q =>
        (
          IO.sleep(5000.milliseconds) >> q.offer(100).debug("add data 100"),
          IO.sleep(5000.milliseconds) >> q.offer(200).debug("add data 200"),
          IO.sleep(5000.milliseconds) >> q.offer(300).debug("add data 300"),
          IO.sleep(5000.milliseconds) >> q.offer(400).debug("add data 400"),
          IO.sleep(5000.milliseconds) >> q.offer(500).debug("add data 500"),
          IO.sleep(5000.milliseconds) >> q.offer(500).debug("add data 500"),
          IO.sleep(5000.milliseconds) >> q.offer(500).debug("add data 500"),
          IO.sleep(5000.milliseconds) >> q.offer(500).debug("add data 500"),
          IO.sleep(5000.milliseconds) >> q.offer(500).debug("add data 500"),
          IO.sleep(5000.milliseconds) >> q.offer(500).debug("add data 500"),
          IO.sleep(5000.milliseconds) >> q.offer(500).debug("add data 500"),
          IO.sleep(5000.milliseconds) >> q.offer(500).debug("add data 500"),
          IO.sleep(5000.milliseconds) >> q.offer(500).debug("add data 500"),
          IO.sleep(5000.milliseconds) >> q.offer(500).debug("add data 500"),
          IO.sleep(5000.milliseconds) >> q.offer(500).debug("add data 500"),
          IO.sleep(5000.milliseconds) >> q.offer(500).debug("add data 500"),
          IO.sleep(5000.milliseconds) >> q.offer(500).debug("add data 500"),
          IO.sleep(5000.milliseconds) >> q.offer(500).debug("add data 500"),
        ).parMapN {
          case _ => "add data"
        }
      } >>
      IO.pure(Console.println(s"end time is ${LocalDateTime.now()}"))

  }
}
