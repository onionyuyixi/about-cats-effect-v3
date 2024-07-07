//package sources.mtl
//
//import cats.Monad
//import cats.data.{Chain, Writer}
//import cats.implicits.catsSyntaxApplicativeId
//import cats.mtl.Tell
//import cats.syntax.all._
//
//object TellCase extends App {
//
//
//  case class ServiceParams(option1: String, option2: Int)
//
//  case class ServiceResult(userId: Int, companies: List[String])
//
//  private def serviceCall[F[_] : Monad](params: ServiceParams): F[ServiceResult] =
//    // a fake call to some external service, replace with real implementation
//    ServiceResult(0, List("Raven Enterprises")).pure[F]
//
//
//  private def serviceCallWithLog[F[_] : Monad](params: ServiceParams)(implicit F: Tell[F, Chain[String]]): F[ServiceResult] =
//    for {
//      _ <- F.tell(Chain.one(show"Call to service with ${params.option1} and ${params.option2}"))
//      result <- serviceCall[F](params)
//      _ <- F.tell(Chain.one(show"Service returned: userId: ${result.userId}; companies: ${result.companies}"))
//    } yield result
//
//
//  serviceCallWithLog[Writer[Chain[String],_]](ServiceParams("AAAA", 100)).run
//
//}
