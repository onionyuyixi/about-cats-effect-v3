package sources.cases

import cats.effect.IO
import cats.effect.IOApp.Simple
import cats.effect.std.MapRef
import cats.implicits.{catsSyntaxParallelSequence1, catsSyntaxParallelTraverse1}

import scala.concurrent.duration.DurationInt

object MapRefCase extends Simple {

  val program: IO[Option[Int]] = for {
    map <- MapRef.ofShardedImmutableMap[IO, String, Int](4)
    _ <- map.setKeyValue("1", 1)
    _ <- map.setKeyValue("2", 2)
    _ <- map.setKeyValue("3", 3)
    _ <- map.setKeyValue("4", 4)
    v <- map.apply("1").get
  } yield v

  override def run: IO[Unit] = program.map(opt => println(opt.get)).void
}

object MapRefCase1 extends Simple {

  private val mapRef: IO[MapRef[IO, String, Option[Int]]] = MapRef.ofShardedImmutableMap[IO, String, Int](4)

  val program: IO[Option[Int]] = mapRef.flatMap { map =>
    List(
      map.setKeyValue("1", 1).debug("put 1"),
      map.setKeyValue("2", 2).debug("put 2"),
      map.setKeyValue("3", 3).debug("put 3"),
      map.setKeyValue("4", 4).debug("put 4")
    ).parSequence *>
      map.apply("1").get.debug("get 1 --->")
  }

  override def run: IO[Unit] = program.void
}
