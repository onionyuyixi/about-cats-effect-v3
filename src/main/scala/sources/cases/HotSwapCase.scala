package sources.cases

import cats.effect
import cats.effect.kernel.Ref
import cats.effect.std.Hotswap
import cats.effect.unsafe.implicits.global
import cats.effect.{IO, Resource}

import scala.language.postfixOps

object HotSwapCase extends App {


  private val ref: IO[Ref[IO, Int]] = effect.Ref.of[IO, Int](100)
  println(ref.flatMap { num =>
    Hotswap.create[IO, Int].allocated.flatMap {
      case (hw, fin) => hw.swap(Resource.eval(num.getAndUpdate(_ + 1))).onCancel(fin)
    } *> num.get.debug("num is ")
  }.flatMap { b =>
    IO.println(s"b=======$b").unsafeRunSync()
    Hotswap.create[IO, Int].use {
      hs => hs.swap(Resource.pure(10000))
    }
  }.unsafeRunSync())


}
