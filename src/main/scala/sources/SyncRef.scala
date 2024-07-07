package sources

import cats.data.State
import cats.effect.Sync

import java.util.concurrent.atomic.AtomicReference
import scala.annotation.tailrec

final class SyncRef[F[_], A](ar: AtomicReference[A])(implicit F: Sync[F]) extends Ref[F, A] {

  def this(a: A)(implicit F: Sync[F]) = this(new AtomicReference(a))

  override def modify[B](f: A => (A, B)): F[B] = {
    @tailrec
    def spin: B = {
      val a = ar.get()
      val (a1, b) = f(a)
      if (!ar.compareAndSet(a, a1)) spin
      else b
    }

    F.delay(spin)
  }

  override def update(f: A => A): F[Unit] = {

    @tailrec
    def spin(): Unit = {
      val a = ar.get()
      val a1 = f(a)
      if (!ar.compareAndSet(a, a1)) spin()
    }

    F.delay(spin())
  }

  override def modifyState[B](state: State[A, B]): F[B] = {
    val f = state.runF.value
    modify(a => f(a).value)

  }

  override def access: F[(A, A => F[Boolean])] =
    F.delay {
      val snapshot = ar.get

      def setter = (a: A) => F.delay(ar.compareAndSet(snapshot, a))

      (snapshot, setter)
    }

  override def tryUpdate(f: A => A): F[Boolean] =
    F.map(tryModify(a => (f(a), ())))(opt => opt.isDefined)

  override def tryModify[B](f: A => (A, B)): F[Option[B]] =
    F.delay {
      val a = ar.get()
      val (a1, b) = f(a)
      if (ar.compareAndSet(a, a1)) Some(b)
      else None
    }

  override def set(a: A): F[Unit] = F.delay(ar.set(a))

  override def get: F[A] = F.delay(ar.get())

  override def getAndSet(a: A): F[A] = F.delay(ar.getAndSet(a))

  override def getAndUpdate(f: A => A): F[A] = {
    @tailrec
    def spin: A = {
      val a = ar.get()
      val u = f(a)
      if (!ar.compareAndSet(a, u)) spin
      else a
    }

    F.delay(spin)
  }
}
