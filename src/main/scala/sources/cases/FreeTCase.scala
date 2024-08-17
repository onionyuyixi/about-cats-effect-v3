package sources.cases

import cats.data.{OptionT, State}
import cats.free.FreeT
import cats.{Eval, ~>}

import scala.util.Try

object FreeTCase extends App {


  sealed abstract class Teletype[A] extends Product with Serializable

  final case class WriteLine(line: String) extends Teletype[Unit]

  final case class ReadLine(promote: String) extends Teletype[String]


  type TeletypeState[A] = State[List[String], A]

  type TeletypeT[M[_], A] = FreeT[Teletype, M, A]


  object TeletypeOps {

    def writeLine(line: String): TeletypeT[TeletypeState, Unit] =
      FreeT.liftF(WriteLine(line))

    def readLine(promote: String): TeletypeT[TeletypeState, String] =
      FreeT.liftF(ReadLine(promote))

    def log(logStr: String): TeletypeT[TeletypeState, Unit] =
      FreeT.liftT[Teletype, TeletypeState, Unit](State.modify((list: List[String]) => logStr :: list))

  }

  def program: TeletypeT[TeletypeState, Unit] =
    for {
      userSaid <- TeletypeOps.readLine("what's up ?!")
      _ <- TeletypeOps.log(s"user said: $userSaid")
      _ <- TeletypeOps.writeLine("thanks, see you soon!")
    } yield ()


  def interpreter: Teletype ~> TeletypeState = new(Teletype ~> TeletypeState) {

    override def apply[A](fa: Teletype[A]): TeletypeState[A] =
      fa match {
        case WriteLine(line) =>
          State.pure[List[String], A](println(line))
        case ReadLine(promote) =>
          println(promote)
          val userInput = "hanging i here"
          State.pure[List[String], A](userInput)
      }

  }

  val result: TeletypeState[Unit] = program.foldMap(interpreter)
  println(result)
  val run: Eval[(List[String], Unit)] = result.run(Nil)
  println(run)
  println(run.value)
}


object FreeTCase1 extends App {

  sealed trait Ctx[A]

  case class Action(value: Int) extends Ctx[Int]


  def op1: FreeT[Ctx, Option, Int] =
    FreeT.liftF(Action(7))

  def op2: FreeT[Ctx, Option, Int] =
    FreeT.liftT(Some(4))

  def op3: FreeT[Ctx, Option, Int] =
    FreeT.pure(1)

  val opComplete: FreeT[Ctx, Option, Int] =
    for {
      a <- op1
      b <- op2
      c <- op3
    } yield a + b + c

  type OptTry[A] = OptionT[Try, A]

  val ctx2Try = new(Ctx ~> OptTry) {
    override def apply[A](fa: Ctx[A]): OptTry[A] =
      fa match {
        case Action(value) => OptionT.liftF(Try(value))
      }
  }

  val opt2Try = new(Option ~> OptTry) {
    override def apply[A](fa: Option[A]): OptTry[A] =
      fa match {
        case Some(value) =>
          OptionT(Try(Option(value)))
        case None =>
          OptionT.none
      }
  }

  println(opComplete)
  private val hoisted: FreeT[Ctx, OptTry, Int] = opComplete.hoist(opt2Try)

  println(hoisted)
  private val optTry: OptTry[Int] = hoisted.foldMap(ctx2Try)
  println(optTry)

  println(optTry.value)


}






























