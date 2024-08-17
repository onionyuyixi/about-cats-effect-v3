package sources.cases

import cats.arrow.FunctionK
import cats.data.{Const, Kleisli, Tuple2K}
import cats.free.FreeApplicative
import cats.implicits.{catsSyntaxTuple2Semigroupal, catsSyntaxTuple3Semigroupal}
import cats.~>
import sources.cases.FreeApplicativeUsage.ValidationOp.{hasNumber, size}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.DurationInt
import scala.concurrent.{Await, Future}

object FreeApplicativeUsage extends App {


  sealed abstract class ValidationOp[A]

  case class Size(size: Int) extends ValidationOp[Boolean]

  case object HasNumber extends ValidationOp[Boolean]

  type ValidationFreeApp[A] = FreeApplicative[ValidationOp, A]

  object ValidationOp {

    def size(size: Int): ValidationFreeApp[Boolean] = FreeApplicative.lift(Size(size))

    def hasNumber: ValidationFreeApp[Boolean] = FreeApplicative.lift(HasNumber)

  }

  val program: ValidationFreeApp[Boolean] = (size(5), hasNumber) mapN { case (l, r) => l && r }

  type FromString[A] = String => A

  val compiler: ValidationOp ~> FromString = new(ValidationOp ~> FromString) {
    override def apply[A](fa: ValidationOp[A]): FromString[A] =
      str => fa match {
        case Size(size) => str.length >= size
        case HasNumber => str.exists(c => "0123456789".contains(c))
      }
  }

  val validator: FromString[Boolean] = program.foldMap(compiler)

  println(validator("12345"))
  println(validator("xdf"))


  type ParValidator[A] = Kleisli[Future, String, A]

  val parCompiler: ValidationOp ~> ParValidator =
    new(ValidationOp ~> ParValidator) {
      override def apply[A](fa: ValidationOp[A]): ParValidator[A] = Kleisli(
        str => fa match {
          case Size(size) =>
            Future {
              str.length >= size
            }
          case HasNumber =>
            Future {
              str.exists("0123456789".contains(_))
            }
        }
      )
    }

  val parValidator: ParValidator[Boolean] = program.foldMap(parCompiler)
  private val eventualBoolean: Future[Boolean] = parValidator("xxxxx")
  private val eventualBoolean1: Future[Boolean] = parValidator("111111")
  println(eventualBoolean.value)
  println(eventualBoolean1.value)
  println(Await.result(eventualBoolean, 1.seconds))
  println(Await.result(eventualBoolean1, 1.seconds))

  type Log[B] = Const[List[String], B]

  private val logCompiler = new FunctionK[ValidationOp, Log] {
    def apply[A](fa: ValidationOp[A]): Log[A] = fa match {
      case Size(size) => Const(List(s"size >= $size"))
      case HasNumber => Const(List("has number"))
    }
  }

  def logValidation[A](validationFreeApp: ValidationFreeApp[A]) =
    validationFreeApp.foldMap(logCompiler).getConst

  println(logValidation(program))

  println(logValidation((hasNumber, size(3)).mapN(_ || _)))
  println(logValidation((hasNumber, size(3), size(1333)).mapN((a, b, c) => a || b || c)))

  type ComposeValidate[A] = Tuple2K[ParValidator, Log, A]

  val composeCompiler = parCompiler and logCompiler

  val composeResult: Tuple2K[ParValidator, Log, Boolean] = program.foldMap(composeCompiler)

  println(composeResult)

}
