package sources.cases

import cats.data.EitherK
import cats.free.Free
import cats.{Id, InjectK, ~>}

import scala.collection.mutable.ListBuffer

object FreeComposeCase extends App {


  sealed trait Interact[A]

   case class Ask(prompt: String) extends Interact[String]

   case class Tell(msg: String) extends Interact[Unit]

  sealed trait DataOp[A]

   case class AddCat(a: String) extends DataOp[Unit]

   case class GetAllCats() extends DataOp[List[String]]

   type CatsApp[A] = EitherK[DataOp, Interact, A]


  class Interacts[F[_]](implicit injectK: InjectK[Interact, F]) {

    def tell(msg: String): Free[F, Unit] = Free.liftInject[F](Tell(msg))

    def ask(prompt: String): Free[F, String] = Free.liftInject[F](Ask(prompt))

  }

  object Interacts {
    implicit def interacts[F[_]](implicit I: InjectK[Interact, F]): Interacts[F] = new Interacts[F]
  }


  class DataSource[F[_]](implicit injectK: InjectK[DataOp, F]) {

    def addCat(a: String): Free[F, Unit] = Free.liftInject[F](AddCat(a))

    def getAllCats: Free[F, List[String]] = Free.liftInject[F](GetAllCats())

  }

  object DataSource {
    implicit def dataSource[F[_]](implicit I: InjectK[DataOp, F]): DataSource[F] =
      new DataSource[F]
  }

  def program(implicit I: Interacts[CatsApp], D: DataSource[CatsApp]): Free[CatsApp, Unit] = {
    import D._
    import I._
    for {
      cat <- ask("What's the kitty's name")
      _ <- addCat(cat)
      cats <- getAllCats
      _ <- tell(cats.toString)
    } yield ()
  }

   def readLine(): String = "snuggles"

   object ConsoleCatsInterpreter extends (Interact ~> Id) {
    override def apply[A](fa: Interact[A]): Id[A] =
      fa match {
        case Ask(prompt) =>
          println(prompt)
          readLine()
        case Tell(msg) =>
          println(msg)
      }
  }


   object InMemoryCatsInterpreter extends (DataOp ~> Id) {

    private [this] val memDataSet = new ListBuffer[String]

    override def apply[A](fa: DataOp[A]): Id[A] =
      fa match {
        case AddCat(a) =>
          memDataSet.append(a)
          ()
        case GetAllCats() =>
          memDataSet.toList
      }
  }

   val interpreter: CatsApp ~> Id = InMemoryCatsInterpreter or ConsoleCatsInterpreter

  program.foldMap(interpreter)

}
