package sources.cases

object ConcurrentCase extends App {


  import cats.effect.IO
  import cats.effect.unsafe.implicits.global

  val action: IO[String] = IO.println("This is only printed once").as("action")

  val x: IO[String] = for {
    memoized <- action.memoize
    res1 <- memoized
    res2 <- memoized
    res3 <- memoized
  } yield res1 ++ res2 ++ res3

  x.unsafeRunSync()


}
