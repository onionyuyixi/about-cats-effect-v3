package sources.cases

import cats.effect.{Deferred, IO}
import cats.implicits.catsSyntaxParallelSequence1

object DeferredCase extends App {

    def start(d:Deferred[IO,Int]) = {
      val attempt: Int => IO[Unit] = n =>d.complete(n).void
      List(
        IO.race(attempt(1), attempt(2)),
        d.get.flatMap { n => IO(println(s"Result: $n")) }
      ).parSequence.void
    }

  val program = {
    for{
      d <- Deferred[IO,Int]
      _ <- start(d)
    }yield ()
  }

  val run: IO[Unit] = program

}
