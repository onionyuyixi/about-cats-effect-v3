package sources.cats_.free

import cats.{Applicative, Functor, Monad, ~>}

import scala.annotation.tailrec

sealed abstract class FreeT[S[_], M[_], A] extends Product with Serializable {

  import FreeT._
  final def map[B](f: A => B)(implicit M: Applicative[M]): FreeT[S, M, B] =
    flatMap(a => pure(f(a)))


  def mapK[N[_]](mn: M ~> N): FreeT[S, N, A] =
    step match {
      case Suspend(ma) =>
        Suspend(mn(ma))
      case FlatMapped(a0, f0) =>
        FlatMapped(a0.mapK(mn), f0.andThen(_.mapK(mn)))
    }

  def flatMap[B](f: A => FreeT[S, M, B]): FreeT[S, M, B] =
    FlatMapped(this, f)

  def hoist[N[_]](mn: M ~> N): FreeT[S, N, A] =
    mapK(mn)

  // 对比Free中的resume Either[S[Free[S, A]], A]
  // 这里多了M的functor 所以这里最后一步 通过Monad将其fold得到最后结果
  // 可以看出FreeT的作用是 将计算委托给Monad
  def resume(implicit S: Functor[S], M: Monad[M]): M[Either[S[FreeT[S, M, A]], A]] = {

    def go(ft: FreeT[S, M, A]): M[Either[FreeT[S, M, A], Either[S[FreeT[S, M, A]], A]]] =
      ft match {
        case Suspend(f) => M.map(f)(as => Right(as.left.map(S.map(_)(pure(_)))))
        case g1 @ FlatMapped(_, _) =>
          g1.a match {
            case Suspend(m1) =>
              M.map(m1) {
                case Right(a) => Left(g1.f(a))
                case Left(fc) => Right(Left(S.map(fc)(g1.f(_))))
              }
            case g2 @ FlatMapped(_, _) => M.pure(Left(g2.a.flatMap(g2.f(_).flatMap(g1.f))))
          }
      }

    M.tailRecM(this)(go)

  }

  @tailrec
  private def step: FreeT[S, M, A] =
    this match {
      case g@FlatMapped(_, _) =>
        g.a match {
          case g0@FlatMapped(_, _) => g0.a.flatMap(a => g0.f(a).flatMap(g.f)).step
          case _ => g
        }
      case x => x
    }


}


object FreeT {

  case class Suspend[S[_], M[_], A](a: M[Either[S[A], A]]) extends FreeT[S, M, A]

  case class FlatMapped[S[_], M[_], A0, B](a0: FreeT[S, M, A0], f0: A0 => FreeT[S, M, B])
    extends FreeT[S, M, B] {
    type A = A0

    def a: FreeT[S, M, A] = a0

    def f: A => FreeT[S, M, B] = f0
  }

  def pure[S[_], M[_], A](value: A)(implicit M: Applicative[M]): FreeT[S, M, A] =
    Suspend(M.pure(Right(value)))

  def defer[S[_], M[_], A](a: M[Either[A, S[FreeT[S, M, A]]]])(implicit M: Applicative[M]) =
    ???


}
