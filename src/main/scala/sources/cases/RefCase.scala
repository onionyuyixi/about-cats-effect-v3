package sources.cases

object RefCase extends App {

}


import cats.effect.kernel.Ref
import cats.effect.unsafe.implicits.global
import cats.effect.{IO, IOApp, Sync}
import cats.syntax.all._

class Worker[F[_]](id: Int, ref: Ref[F, Int])(implicit F: Sync[F]) {

  private def putStrLn(value: String): F[Unit] =
    F.blocking(println(value))

  def start: F[Unit] =
    for {
      c1 <- ref.get
      _ <- putStrLn(show"Worker #$id >> $c1")
      c2 <- ref.updateAndGet(x => x + 1)
      _ <- putStrLn(show"Worker #$id >> $c2")
    } yield ()
}

object RefExample extends IOApp.Simple {

  val run: IO[Unit] =
    for {
      ref <- Ref[IO].of(0)
      w1 = new Worker[IO](1, ref)
      w2 = new Worker[IO](2, ref)
      w3 = new Worker[IO](3, ref)
      _ <- List(
        w1.start,
        w2.start,
        w3.start,
      ).parSequence.void
    } yield ()
}


object RefExample1 extends IOApp.Simple {

  val program = IO.unit >> Ref[IO].of(10).flatMap { ref =>
    val w1 = new Worker[IO](1, ref)
    val w2 = new Worker[IO](2, ref)
    val w3 = new Worker[IO](3, ref)
    List(
      w1.start,
      w2.start,
      w3.start,
    ).parSequence.void
  }

  val run: IO[Unit] = program.void
}
