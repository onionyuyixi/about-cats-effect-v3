package sources

import cats.effect.{Async, GenConcurrent}
import cats.implicits.{catsSyntaxFlatMapOps, toFunctorOps}
import cats.~>

import java.util.concurrent.atomic.AtomicReference
import scala.annotation.tailrec
import scala.collection.immutable.LongMap

trait DeferredSource[F[_], A] {

  def get: F[A]

  def tryGet: F[Option[A]]

}

trait DeferredSink[F[_], A] {

  def complete(a: A): F[Boolean]

}


abstract class Deferred[F[_], A] extends DeferredSource[F, A] with DeferredSink[F, A] {


  def mapK[G[_]](f: F ~> G): Deferred[G, A] =
    ???
}

object Deferred {

  def apply[F[_], A](implicit F: GenConcurrent[F, ?]) =
    F.deferred[A]

  def unsafe[F[_] : Async, A] = ???

  sealed abstract private class State[A]

  private object State {

    final case class Set[A](a: A) extends State[A]

    final case class Unset[A](readers: LongMap[A => Unit], nextId: Long) extends State[A]

    val initialId = 1L;

    val dummyId = 0L

  }

  final class AsyncDeferred[F[_], A](implicit F: Async[F]) extends Deferred[F, A] {

    private val ref = new AtomicReference[State[A]](
      State.Unset(LongMap.empty, State.initialId)
    )


    override def get: F[A] = ???

    override def tryGet: F[Option[A]] = F.delay {
      ref.get() match {
        case State.Set(a) => Some(a)
        case State.Unset(readers, nextId) => None
      }
    }

    override def complete(a: A): F[Boolean] = {

      def notifyReaders(readers: LongMap[A => Unit]): F[Unit] = {
        val iterator = readers.valuesIterator
        var acc = F.unit
        while (iterator.hasNext) {
          val next = iterator.next()
          val task = F.delay(next(a))
          acc = acc >> task
        }
        acc
      }

      @tailrec
      def loop(): F[Boolean] =
        ref.get() match {
          case State.Set(_) => F.pure(false)
          case s@State.Unset(readers, _) =>
            val update = State.Set(a)
            if (!ref.compareAndSet(s, update)) loop()
            else {
              val notify = if (readers.isEmpty) F.unit
              else notifyReaders(readers)
              notify.as(true)
            }
        }

      F.uncancelable(_ => F.defer(loop()))
    }
  }


}
