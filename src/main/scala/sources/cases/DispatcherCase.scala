package sources.cases

import cats.effect.IO
import cats.effect.std.{Dispatcher, Queue}
import cats.effect.unsafe.implicits.global

object DispatcherCase extends App {


  abstract class ImpureInterface {

    def onMessage(msg: String): Unit

    def init(): Unit = {
      onMessage("init")
    }

  }


  val program = for {
    queue <- Queue.unbounded[IO, String]
    impure = new ImpureInterface {
      override def onMessage(msg: String): Unit = queue.offer(msg)
    }
    _ <- IO.delay(impure.init())
    value <- queue.tryTake
  } yield value match {
    case Some(value) => println(s"found value in queue $value")
    // 代码走到了这里 其原因乃是ImpureInterface没有引入effect
    case None => println(s"value not found in queue")
  }

  program.unsafeRunSync()


  val program1: IO[Unit] = Dispatcher.sequential[IO] use { dispatcher =>
    for {
      queue <- Queue.unbounded[IO, String]
      impureInterface <-
        IO {
          new ImpureInterface {
            override def onMessage(msg: String): Unit = {
              // 通过dispatcher引入了effect
              dispatcher.unsafeRunSync(queue.offer(msg))
            }
          }
        }
      _ <- IO(impureInterface.init())
      value <- queue.tryTake
    } yield value match {
      case Some(v) => println(s"Value found in queue! $v")
      case None => println("Value not found in queue :(")
    }
  }

  program1.unsafeRunSync()
}
