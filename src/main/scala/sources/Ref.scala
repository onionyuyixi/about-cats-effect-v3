package sources

import cats.data.State
import cats.effect.kernel.Ref.ApplyBuilders
import cats.effect.{GenConcurrent, MonadCancel, Poll, Sync}
import cats.{Contravariant, Functor, Monoid, ~>}
import sources.Ref.TransformedRef

trait RefSource[F[_], A] extends Serializable {


  def get: F[A]
}

trait RefSink[F[_], A] extends Serializable {


  def set(a: A): F[Unit]
}

abstract class Ref[F[_], A] extends RefSource[F, A] with RefSink[F, A] {

  def modify[B](f: A => (A, B)): F[B]

  def update(f: A => A): F[Unit]

  def getAndUpdate(f: A => A): F[A] =
    modify(a => (f(a), a))

  def getAndSet(a: A): F[A] =
    getAndUpdate(_ => a)

  def updateAndGet(f: A => A): F[A] =
    modify {
      a =>
        val newA = f(a)
        (newA, newA)
    }

  def flatModify[B](f: A => (A, F[B]))(implicit F: MonadCancel[F, _]): F[B] =
    F.uncancelable(_ => F.flatten(modify(f)))

  def modifyState[B](state: State[A, B]): F[B]

  def flatModifyState[B](state: State[A, F[B]])(implicit F: MonadCancel[F, _]): F[B] =
    F.uncancelable(_ => F.flatten(modifyState(state)))

  def flatModifyStateFull[B](state: Poll[F] => State[A, F[B]])(implicit F: MonadCancel[F, _]): F[B] =
    F.uncancelable {
      poll =>
        F.flatten(modifyState(state(poll)))
    }

  def mapK[G[_]](f: F ~> G)(implicit F: Functor[F]): Ref[G, A] =
    new TransformedRef(this, f)

  def access: F[(A, A => F[Boolean])]

  def tryUpdate(f: A => A): F[Boolean]

  def tryModify[B](f: A => (A, B)): F[Option[B]]


}

object Ref {

  trait Make[F[_]] {
    def refOf[A](a: A): F[Ref[F, A]]
  }


  // 创建并行ref
  private trait MakeInstances extends MakeLowPriorityInstances {
    implicit def concurrentInstance[F[_]](implicit F: GenConcurrent[F,_]): Make[F] =
      new Make[F] {
        override def refOf[A](a: A): F[Ref[F, A]] =
          ??? //F.ref(a)
      }
  }

  // 创建同步ref
  private trait MakeLowPriorityInstances {
    implicit def syncInstance[F[_]](implicit F: Sync[F]): Make[F] =
      new Make[F] {
        override def refOf[A](a: A): F[Ref[F, A]] =
          F.delay(unsafe(a))
      }
  }

  def apply[F[_]](implicit make:Make[F]):ApplyBuilders[F] =
    ??? // new ApplyBuilders(make)

  def of[F[_],A](a:A) (implicit mk:Make[F]) =
    ??? // mk.refOf(a)

  def empty[F[_]:Make,A:Monoid]:F[Ref[F,A]] =
    of(Monoid[A].empty)

  def unsafe[F[_], A](a: A)(implicit F: Sync[F]) =
    new SyncRef(a)

  import cats.FlatMap
  def ofEffect[F[_]: Make: FlatMap,A](fa:F[A]):F[Ref[F,A]] =
    ??? // FlatMap[F].flatMap(fa)(of(_))



  final private class TransformedRef[F[_], G[_], A](
                                                     underlying: Ref[F, A],
                                                     trans: F ~> G)(
                                                     implicit F: Functor[F]
                                                   ) extends Ref[G, A] {

    override def modify[B](f: A => (A, B)): G[B] = ???

    override def update(f: A => A): G[Unit] = ???

    override def modifyState[B](state: State[A, B]): G[B] = ???

    override def access: G[(A, A => G[Boolean])] = ???

    override def tryUpdate(f: A => A): G[Boolean] = ???

    override def tryModify[B](f: A => (A, B)): G[Option[B]] = ???

    override def set(a: A): G[Unit] = ???

    override def get: G[A] = ???
  }
}
