package sources.cases

import cats.effect.unsafe.implicits.global
import cats.effect.{Deferred, IO}
import cats.implicits.{catsSyntaxApplyOps, catsSyntaxParallelSequence1}

object DeferredCase extends App {

  def start(d: Deferred[IO, Int]) = {
    val attempt: Int => IO[Unit] = n => d.complete(n).void
    List(
      IO.race(attempt(1), attempt(2)),
      d.get.flatMap { n => IO(println(s"Result: $n")) }
    ).parSequence.void
  }

  val program = {
    for {
      d <- Deferred[IO, Int]
      _ <- start(d)
    } yield ()
  }

  val run: IO[Unit] = program

}


object DeferredCase1 extends App {


  IO.deferred[Deferred[IO, Int]].flatMap { outDefer =>
    IO.deferred[Int].flatMap { innerDefer =>
      //这里outDefer其实容纳了innerDefer的complete数据
      outDefer.complete(innerDefer) *> innerDefer.complete(10)
    } *> outDefer.get.flatMap(_.tryGet).flatMap(a=>IO.println(s"out data $a"))  // out data Some(10)
  }.unsafeRunSync()


}