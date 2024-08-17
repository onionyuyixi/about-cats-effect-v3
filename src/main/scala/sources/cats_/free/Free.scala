package sources.cats_.free

import cats.{Comonad, Functor, Id, InjectK, Monad, ~>}

import scala.annotation.tailrec

sealed abstract class Free[S[_], A] extends Product with Serializable {

  import Free._
  final def map[B](f: A => B): Free[S, B] =
    flatMap(a => Pure(f(a)))

  final def flatMap[B](f: A => Free[S, B]): Free[S, B] =
    FlatMapped(this, f)

  final def mapK[T[_]](f: S ~> T): Free[T, A] = ???

  final def fold[B](rightFunc: A => B, leftFunc: S[Free[S, A]] => B)(implicit F: Functor[S]): B =
    resume.fold(leftFunc, rightFunc)

  final def step: Free[S, A] =
    this match {
      case FlatMapped(FlatMapped(c, f), g) =>
        c.flatMap(cc => f(cc).flatMap(g))
      case FlatMapped(Pure(a), f) =>
        f(a).step
      case FlatMapped(sa, f) =>
        // 不过这样一来 其实表达的就是其本身 FlatMapped(sa: Suspend[S, A], f)
        // 所以cats-free的源码里 没有这一步骤
        sa.flatMap(f)
      case x => x
    }

  @tailrec
  final def resume(implicit F: Functor[S]): Either[S[Free[S, A]], A] =
    this match {
      case Pure(a) => Right(a)
      case Suspend(sa) =>
        val value: S[Free[S, A]] = F.map(sa)(Pure(_))
        Left(value)
      case flatMapped: FlatMapped[S, A, A] =>
        flatMapped.prev match {
          case Pure(a) =>
            val value: Free[S, A] = flatMapped.f(a)
            value.resume
          case Suspend(sa) =>
            val value: S[Free[S, A]] = F.map(sa)(flatMapped.f)
            Left(value)
          case FlatMapped(d, g) =>
            d.flatMap(dd => g(dd).flatMap(flatMapped.f)).resume
        }
    }

  final def go(f: S[Free[S, A]] => Free[S, A])(implicit F: Functor[S]): A = {

    @tailrec
    def loop(t: Free[S, A]): A =
      t.resume match {
        case Right(value) => value
        case Left(value) => loop(f(value))
      }

    loop(this)
  }

  final def run(implicit Co: Comonad[S]): A =
    go(Co.extract)

  final def runM[M[_]](f: S[Free[S, A]] => M[Free[S, A]])(implicit F: Functor[S], M: Monad[M]): M[A] = {

    def step(t: S[Free[S, A]]): M[Either[S[Free[S, A]], A]] = {
      val func: (Free[S, A] => Either[S[Free[S, A]], A]) => M[Either[S[Free[S, A]], A]] = M.map(f(t))
      func(_.resume)
    }

    resume match {
      case Right(value) => M.pure(value)
      case Left(value) => M.tailRecM(value)(step)
    }

  }


  final def runTailRec(implicit M: Monad[S]): S[A] = {

    def step(rma: Free[S, A]): S[Either[Free[S, A], A]] =
      rma match {
        case Pure(a) =>
          M.pure(Right(a))
        case Suspend(sa) =>
          M.map(sa)(a => Right(a))
        case FlatMapped(prev, f) =>
          prev match {
            case Pure(a) => M.pure(Left(f(a)))
            case Suspend(sa) => M.map(sa)(a => Left(f(a)))
            case FlatMapped(pprev, ff) =>
              M.pure(Left(pprev.flatMap(w => ff(w).flatMap(f))))
          }
      }

    M.tailRecM(this)(step)
  }

  def foldMap[M[_]](f: S ~> M)(implicit M: Monad[M]): M[A] =
    M.tailRecM(this) { free =>
      free.step match {
        case Pure(a) => M.pure(Right(a))
        case Suspend(sa) =>
          M.map(f(sa))(Right(_))
        case FlatMapped(prev, ff) =>
          M.map(prev.foldMap(f))(cc => Left(ff(cc)))
      }
    }

  final def compile[T[_]](f: S ~> T): Free[T, A] =
    mapK(f)

  final def inject[G[_]](implicit ev: InjectK[S, G]): Free[G, A] =
    mapK(new(S ~> G) {
      override def apply[A](fa: S[A]): G[A] = ev.inj(fa)
    })


}

object Free {

  final private case class Pure[S[_], A](a: A) extends Free[S, A]

  final private case class Suspend[S[_], A](sa: S[A]) extends Free[S, A]

  final private case class FlatMapped[S[_], A, B](prev: Free[S, A], f: A => Free[S, B]) extends Free[S, B]

  def pure[S[_], A](a: A): Free[S, A] = Pure(a)

  def liftF[S[_], A](value: S[A]): Free[S, A] = Suspend(value)

  def liftK[F[_]]: F ~> ({type l[X] = Free[F, X]})#l =
    new(F ~> ({type l[X] = Free[F, X]})#l) {
      override def apply[A](fa: F[A]): Free[F, A] =
        liftF[F, A](fa)
    }

  private def liftId[F[_]]: Id ~> ({type l[X] = Free[F, X]})#l =
    new(Id ~> ({type l[X] = Free[F, X]})#l) {
      override def apply[A](fa: Id[A]): Free[F, A] = liftId(fa)
    }

  def roll[F[_],A](value:F[Free[F,A]]):Free[F, A] =
    liftF(value).flatMap(identity)
































}
