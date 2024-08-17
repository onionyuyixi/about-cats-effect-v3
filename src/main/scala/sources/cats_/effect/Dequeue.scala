package sources.cats_.effect

import cats.effect.implicits.monadCancelOps_
import cats.effect.kernel.{Deferred, GenConcurrent, Ref}
import cats.effect.std.{Queue, QueueSink, QueueSource}
import cats.implicits.{catsSyntaxApplyOps, catsSyntaxIfM, toFlatMapOps, toFunctorOps}
import cats.{Contravariant, Functor, Monad, ~>}

import scala.collection.immutable.{Queue => ScalaQueue}

trait Dequeue[F[_], A] extends Queue[F, A] with DequeueSource[F, A] with DequeueSink[F, A] {
  self =>
  def reverse: F[Unit]

  override def mapK[G[_]](f: F ~> G): Dequeue[G, A] =
    new Dequeue[G, A] {
      def offerBack(a: A): G[Unit] = f(self.offerBack(a))

      def tryOfferBack(a: A): G[Boolean] = f(self.tryOfferBack(a))

      def takeBack: G[A] = f(self.takeBack)

      def tryTakeBack: G[Option[A]] = f(self.tryTakeBack)

      def offerFront(a: A): G[Unit] = f(self.offerFront(a))

      def tryOfferFront(a: A): G[Boolean] = f(self.tryOfferFront(a))

      def takeFront: G[A] = f(self.takeFront)

      def tryTakeFront: G[Option[A]] = f(self.tryTakeFront)

      def reverse: G[Unit] = f(self.reverse)

      def size: G[Int] = f(self.size)
    }

}

object Dequeue {

  private def assertNonNegative(capacity: Int): Unit =
    require(capacity >= 0, s"Bounded queue capacity must be non-negative, was: $capacity")

  def bounded[F[_], A](capacity: Int)(implicit F: GenConcurrent[F, ?]): F[Dequeue[F, ?]] = {
    assertNonNegative(capacity)
    F.ref(State.empty[F, A]).map(refState => new BoundedDequeue[F, A](capacity, refState))
  }


  private class BoundedDequeue[F[_], A](capacity: Int, state: Ref[F, State[F, A]])(
    implicit F: GenConcurrent[F, ?])
    extends Dequeue[F, A] {


    override def reverse: F[Unit] = ???

    override def offerBack(a: A): F[Unit] = ???

    override def tryOfferBack(a: A): F[Boolean] = ???

    override def offerFront(a: A): F[Unit] = ???

    override def tryOfferFront(a: A): F[Boolean] = ???

    override def takeBack: F[A] = ???

    override def tryTakeBack: F[Option[A]] = ???

    override def takeFront: F[A] = ???

    override def tryTakeFront: F[Option[A]] = ???

    override def size: F[Int] = ???


    def _offer(a: A, update: BankersQueue[A] => BankersQueue[A]): F[Unit] = {
      F.uncancelable { poll =>
        F.deferred[Unit].flatMap { offerer =>
          state.modify {

            // 此时queue中没有数据 taker还在等待
            case State(queue, size, takers, offerers) if takers.nonEmpty =>
              val (head, tail) = takers.dequeue
              State(update(queue), size, tail, offerers) -> head.complete(()).void

            // queue中存在数据 但还未超过容量
            case State(queue, size, takers, offerers) if size < capacity =>
              State(update(queue), size + 1, takers, offerers) -> F.unit

            // 数据已经超过指定容量
            case s =>
              val State(queue, size, takers, offerers) = s

              val cancelledWhenOfferFail = state.modify { s1 =>
                val offerers2 = s1.offerers.filter(_ ne offerer)
                if (offerers2.isEmpty) {
                  s1.copy(offerers = offerers2) -> F.unit
                } else {
                  val (headOfferer, rest) = offerers2.dequeue
                  s1.copy(offerers = rest) -> headOfferer.complete(()).void
                }
              }

              // 将offerer加入offerers 同时等待当前的offerer完成 然后才去添加数据a
              State(queue, size, takers, offerers.enqueue(offerer)) ->
                (poll(offerer.get) *> poll(_offer(a, update)))
                  .onCancel(cancelledWhenOfferFail.flatten)
          }

        }.flatten
      }
    }

    def _take(dequeue: BankersQueue[A] => (BankersQueue[A], Option[A])): F[A] = {
      F.uncancelable { poll =>
        F.deferred[Unit].flatMap { taker =>
          state.modify {

            // 没有等待offerer 同时queue中有数据 直接修改队列数据
            case State(queue, size, takers, offerers) if offerers.isEmpty && queue.nonEmpty =>
              val (rest, ma) = dequeue(queue)
              State(rest, size - 1, takers, offerers) -> F.pure(ma.get)

            // 队列中有数据
            case State(queue, size, takers, offerers) if queue.nonEmpty =>
              val (rest, ma) = dequeue(queue)
              val (h, t) = offerers.dequeue
              State(rest, size - 1, takers, t) -> h.complete(()).as(ma.get)

            // queue中没有数据 同时还有offerers
            case State(queue, size, takers, offerers) =>

              val cancelledWhenTakeFail = state.modify { s1 =>
                val takers2 = s1.takers.filter(_ ne taker)
                if (takers2.isEmpty) {
                  s1.copy(takers = takers2) -> F.unit
                } else {
                  val (headTaker, rest) = takers2.dequeue
                  s1.copy(takers = rest) -> headTaker.complete(()).void
                }
              }.flatten

              val notifyNextTaker =
                state modify { s =>
                  // 本没有taker
                  if (s.takers.isEmpty) {
                    s -> F.unit
                  } else {
                    val (taker, rest) = s.takers.dequeue
                    s.copy(takers = rest) -> taker.complete(()).void
                  }
                }

              val await = poll(taker.get).onCancel(cancelledWhenTakeFail) *>
                poll(_take(dequeue)).onCancel(notifyNextTaker.flatten)

              val (fulfill, offerers2) =
                if (offerers.isEmpty) { // 没有需要完成的offer
                  (await, offerers)
                } else {
                  val (headrelease, rest) = offerers.dequeue
                  (headrelease.complete(()) *> await, rest)
                }

              State(queue, size, takers.enqueue(taker), offerers2) -> fulfill
          }

        }.flatten
      }
    }


  }


}


private final case class State[F[_], A](
                                         queue: BankersQueue[A],
                                         size: Int,
                                         takers: ScalaQueue[Deferred[F, Unit]],
                                         offerers: ScalaQueue[Deferred[F, Unit]])

private object State {
  def empty[F[_], A]: State[F, A] =
    State(BankersQueue.empty, 0, ScalaQueue.empty, ScalaQueue.empty)
}


trait DequeueSource[F[_], A] extends QueueSource[F, A] {

  def takeBack: F[A]

  def tryTakeBack: F[Option[A]]

  def tryTakeBackN(maxN: Option[Int])(implicit F: Monad[F]): F[List[A]] =
    _tryTakeN(tryTakeBack)(maxN)

  def takeFront: F[A]

  def tryTakeFront: F[Option[A]]

  def tryTakeFrontN(maxN: Option[Int])(implicit F: Monad[F]): F[List[A]] =
    _tryTakeN(tryTakeFront)(maxN)

  override def take: F[A] = takeFront

  override def tryTake: F[Option[A]] = tryTakeFront

  private def _tryTakeN(_tryTake: F[Option[A]])(maxN: Option[Int])(
    implicit F: Monad[F]): F[List[A]] = {

    DequeueSource.assertMaxNPositive(maxN)

    def loop(i: Int, limit: Int, acc: List[A]): F[List[A]] =
      if (i >= limit)
        F.pure(acc.reverse)
      else
        _tryTake flatMap {
          case Some(a) => loop(i + 1, limit, a :: acc)
          case None => F.pure(acc.reverse)
        }

    maxN match {
      case Some(limit) => loop(0, limit, Nil)
      case None => loop(0, Int.MaxValue, Nil)
    }
  }

}

object DequeueSource {
  private def assertMaxNPositive(maxN: Option[Int]): Unit = maxN match {
    case Some(n) if n <= 0 =>
      throw new IllegalArgumentException(s"Provided maxN parameter must be positive, was $n")
    case _ => ()
  }

  implicit def catsFunctorForDequeueSource[F[_] : Functor]: Functor[({type l[A] = DequeueSource[F, A]})#l] =
    new Functor[({type l[A] = DequeueSource[F, A]})#l] {
      override def map[A, B](fa: DequeueSource[F, A])(f: A => B): DequeueSource[F, B] =
        new DequeueSource[F, B] {
          override def takeBack: F[B] =
            fa.takeBack.map(f)

          override def tryTakeBack: F[Option[B]] =
            fa.tryTakeBack.map(_.map(f))

          override def takeFront: F[B] =
            fa.takeFront.map(f)

          override def tryTakeFront: F[Option[B]] =
            fa.tryTakeFront.map(_.map(f))

          override def size: F[Int] =
            fa.size
        }
    }
}

trait DequeueSink[F[_], A] extends QueueSink[F, A] {

  def offerBack(a: A): F[Unit]

  def tryOfferBack(a: A): F[Boolean]

  def tryOfferBackN(list: List[A])(implicit F: Monad[F]): F[List[A]] =
    _tryOfferN(list)(tryOfferBack)

  def offerFront(a: A): F[Unit]

  def tryOfferFront(a: A): F[Boolean]

  def tryOfferFrontN(list: List[A])(implicit F: Monad[F]): F[List[A]] =
    _tryOfferN(list)(tryOfferFront)

  override def offer(a: A): F[Unit] = offerBack(a)

  override def tryOffer(a: A): F[Boolean] = tryOfferBack(a)

  private def _tryOfferN(list: List[A])(_tryOffer: A => F[Boolean])(
    implicit F: Monad[F]): F[List[A]] = list match {
    case Nil => F.pure(list)
    case h :: t =>
      _tryOffer(h).ifM(
        tryOfferN(t),
        F.pure(list)
      )
  }

}

object DequeueSink {
  implicit def catsContravariantForDequeueSink[F[_]]: Contravariant[({type l[A] = DequeueSink[F, A]})#l] =
    new Contravariant[({type l[A] = DequeueSink[F, A]})#l] {
      override def contramap[A, B](fa: DequeueSink[F, A])(f: B => A): DequeueSink[F, B] =
        new DequeueSink[F, B] {
          override def offerBack(b: B): F[Unit] =
            fa.offerBack(f(b))

          override def tryOfferBack(b: B): F[Boolean] =
            fa.tryOfferBack(f(b))

          override def offerFront(b: B): F[Unit] =
            fa.offerFront(f(b))

          override def tryOfferFront(b: B): F[Boolean] =
            fa.tryOfferFront(f(b))
        }

    }

}