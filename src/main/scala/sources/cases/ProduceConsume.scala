package sources.cases

import cats.effect._
import cats.effect.implicits.monadCancelOps_
import cats.effect.kernel.Deferred
import cats.effect.std.Console
import cats.implicits.catsSyntaxParallelSequence1

import scala.collection.immutable.Queue

object ProduceConsume extends IOApp {

  import cats.syntax.all._

  override def run(args: List[String]): IO[ExitCode] =
    for {
      queueR <- Ref.of[IO, Queue[Int]](Queue.empty)
      res <- (Consumer.consumer(queueR), Producer.producer(queueR, 0))
        .parMapN((_, _) => ExitCode.Success)
        .handleErrorWith { error =>
          Console[IO].errorln(s"Error caught : ${error.getMessage}").as(ExitCode.Error)
        }
    } yield res

}

object ProduceConsumeFiberFork extends IOApp {

  override def run(args: List[String]): IO[ExitCode] =
    for {
      queueR <- Ref.of[IO, Queue[Int]](Queue.empty)
      produceFiber <- Producer.producer(queueR, 0).start
      consumerFiber <- Consumer.consumer(queueR).start
      _ <- produceFiber.join
      _ <- consumerFiber.join
    } yield ExitCode.Error

}

object ProduceConsumeDefer extends IOApp {

  override def run(args: List[String]): IO[ExitCode] =
    for {
      stateR <- Ref.of[IO, State[IO, Int]](State.empty[IO, Int])
      counterR <- Ref.of[IO, Int](1)
      producers = List.range(1, 11).map(Producer.producerDefer(_, counterR, stateR))
      consumers = List.range(1, 11).map(Consumer.consumerDefer(_, stateR))
      res <- (producers ++ consumers)
        .parSequence
        .as(ExitCode.Success)
        .handleErrorWith(err => {
          Console[IO].errorln(s"Error caught: ${err.getMessage}").as(ExitCode.Error)
        })
    } yield res

}


object ProduceConsumeDeferBound extends IOApp {

  override def run(args: List[String]): IO[ExitCode] =
    for {
      stateR <- Ref.of[IO, StateBound[IO, Int]](StateBound.empty[IO, Int](100))
      counterR <- Ref.of[IO, Int](1)
      producers = List.range(1, 11).map(Producer.producerDeferBound(_, counterR, stateR))
      consumers = List.range(1, 11).map(Consumer.consumerDeferBound(_, stateR))
      res <- (producers ++ consumers)
        .parSequence
        .as(ExitCode.Success)
        .handleErrorWith(err => {
          Console[IO].errorln(s"producer error caught: ${err.getMessage}").as(ExitCode.Error)
        })
    } yield res

}


object ProduceConsumeDeferBoundCancelable extends IOApp {

  override def run(args: List[String]): IO[ExitCode] =
    for {
      stateR <- Ref.of[IO, StateBound[IO, Int]](StateBound.empty[IO, Int](100))
      counterR <- Ref.of[IO, Int](1)
      producers = List.range(1, 11).map(Producer.producerDeferBoundCancelable(_, counterR, stateR))
      consumers = List.range(1, 11).map(Consumer.consumerDeferBoundCancelable(_, stateR))
      res <- (producers ++ consumers)
        .parSequence
        .as(ExitCode.Success)
        .handleErrorWith(err => {
          Console[IO].errorln(s"producer error caught: ${err.getMessage}").as(ExitCode.Error)
        })
    } yield res

}

object Producer {

  import cats.syntax.all._

  def producer[F[_] : Sync : Console](queueR: Ref[F, Queue[Int]], counter: Int): F[Unit] =
    for {
      _ <- if (counter % 10000 == 0) Console[F].println(s"Produced $counter items")
      else Sync[F].unit
      _ <- queueR.getAndUpdate(_.enqueue(counter + 1))
      _ <- producer[F](queueR, counter + 1)
    } yield ()

  def producerDefer[F[_] : Async : Console](id: Int, counterR: Ref[F, Int], stateR: Ref[F, State[F, Int]]): F[Unit] = {

    def offer(i: Int): F[Unit] =
      stateR.modify {
        case State(queue, takers) if takers.nonEmpty =>
          val (taker, rest) = takers.dequeue
          State(queue, rest) -> taker.complete(i).void
        case State(queue, takers) =>
          State(queue.enqueue(i), takers) -> Sync[F].unit
      }.flatten

    for {
      i <- counterR.getAndUpdate(_ + 1)
      _ <- offer(i)
      _ <- if (i % 10000 == 0) Console[F].println(s"Producer $id has reached $i items") else Sync[F].unit
      _ <- producerDefer(id, counterR, stateR)
    } yield ()

  }

  def producerDeferBound[F[_] : Async : Console](id: Int, counterR: Ref[F, Int], stateR: Ref[F, StateBound[F, Int]]): F[Unit] = {

    def offer(i: Int): F[Unit] =
      Deferred[F, Unit].flatMap[Unit] { offerer =>
        stateR.modify {
          case StateBound(queue, capacity, takers, offers) if takers.nonEmpty =>
            val (taker, rest) = takers.dequeue
            StateBound(queue, capacity, rest, offers) -> taker.complete(i).void
          case StateBound(queue, capacity, takers, offers) if queue.size < capacity =>
            StateBound(queue.enqueue(i), capacity, takers, offers) -> Async[F].unit
          case StateBound(queue, capacity, takers, offers) =>
            StateBound(queue, capacity, takers, offers.enqueue(i -> offerer)) -> offerer.get
        }.flatten
      }

    for {
      i <- counterR.getAndUpdate(_ + 1)
      _ <- offer(i)
      _ <- if (i % 10000 == 0) Console[F].println(s"Producer $id has reached $i items") else Async[F].unit
      _ <- producerDeferBound(id, counterR, stateR)
    } yield ()

  }

  def producerDeferBoundCancelable[F[_] : Async : Console](id: Int, counterR: Ref[F, Int], stateR: Ref[F, StateBound[F, Int]]): F[Unit] = {

    def offer(i: Int): F[Unit] =
      Deferred[F, Unit].flatMap[Unit] { offerer =>
        Async[F].uncancelable { poll =>
          stateR.modify {
            case StateBound(queue, capacity, takers, offers) if takers.nonEmpty =>
              val (taker, rest) = takers.dequeue
              StateBound(queue, capacity, rest, offers) -> taker.complete(i).void
            case StateBound(queue, capacity, takers, offers) if queue.size < capacity =>
              StateBound(queue.enqueue(i), capacity, takers, offers) -> Async[F].unit
            case StateBound(queue, capacity, takers, offers) =>
              val cleanup: F[Unit] = stateR.update { s => s.copy(offers = s.offers.filter(_._2 ne offerer)) }
              StateBound(queue, capacity, takers, offers.enqueue(i -> offerer)) -> poll(offerer.get).onCancel(cleanup)
          }.flatten

        }

      }

    for {
      i <- counterR.getAndUpdate(_ + 1)
      _ <- offer(i)
      _ <- if (i % 10000 == 0) Console[F].println(s"Producer $id has reached $i items") else Async[F].unit
      _ <- producerDeferBound(id, counterR, stateR)
    } yield ()

  }

}

object Consumer {

  import cats.syntax.all._

  def consumer[F[_] : Sync : Console](queueR: Ref[F, Queue[Int]]): F[Unit] =
    for {
      io <- queueR.modify { queue =>
        queue.dequeueOption.fold((queue, Option.empty[Int])) {
          case (i, ints) => (ints, Option(i))
        }
      }
      _ <- if (io.exists(_ % 10000 == 0)) Console[F].println(s" Consumed ${io.get} items")
      else Sync[F].unit

      _ <- consumer(queueR)
    } yield ()

  def consumerDefer[F[_] : Async : Console](id: Int, stateR: Ref[F, State[F, Int]]): F[Unit] = {
    val take: F[Int] =
      Deferred[F, Int].flatMap { (taker: Deferred[F, Int]) =>
        stateR.modify {
          case State(queue, takers) if queue.nonEmpty =>
            val (i, rest) = queue.dequeue
            State(rest, takers) -> Async[F].pure(i)
          case State(queue, takers) =>
            State(queue, takers.enqueue(taker)) -> taker.get
        }.flatten
      }
    for {
      i <- take
      _ <- if (i % 1000 == 0) Console[F].errorln(s" Consumer $id has reached $i items")
      else Async[F].unit
      _ <- consumerDefer(id, stateR)
    } yield ()

  }


  def consumerDeferBound[F[_] : Async : Console](id: Int, stateR: Ref[F, StateBound[F, Int]]): F[Unit] = {
    val take: F[Int] = Deferred[F, Int].flatMap { taker =>
      stateR.modify {
        case StateBound(queue, capacity, takers, offers) if queue.nonEmpty && offers.isEmpty =>
          val (i, rest) = queue.dequeue
          StateBound(rest, capacity, takers, offers) -> Async[F].pure(i)

        case StateBound(queue, capacity, takers, offers) if queue.nonEmpty && offers.nonEmpty =>
          val (i, rest) = queue.dequeue
          val ((move, release), tail) = offers.dequeue
          StateBound(rest.enqueue(move), capacity, takers, tail) -> release.complete(()).as(i)

        case StateBound(queue, capacity, takers, offers) if queue.isEmpty && offers.nonEmpty =>
          val ((i, release), rest) = offers.dequeue
          StateBound(queue, capacity, takers, rest) -> release.complete(()).as(i)

        case StateBound(queue, capacity, takers, offers) if queue.isEmpty && offers.isEmpty =>
          StateBound(queue, capacity, takers.enqueue(taker), offers) -> taker.get
      }
    }.flatten

    for {
      i <- take
      _ <- if (i % 10000 == 0) Console[F].println(s"Consumer $id has reached $i items") else Async[F].unit
      _ <- consumerDeferBound(id, stateR)
    } yield ()

  }


  def consumerDeferBoundCancelable[F[_] : Async : Console](id: Int, stateR: Ref[F, StateBound[F, Int]]): F[Unit] = {
    val take: F[Int] =
      Deferred[F, Int].flatMap { taker =>
        Async[F].uncancelable { poll =>
          stateR.modify {
            case StateBound(queue, capacity, takers, offerers) if queue.nonEmpty && offerers.isEmpty =>
              val (i, rest) = queue.dequeue
              StateBound(rest, capacity, takers, offerers) -> Async[F].pure(i)
            case StateBound(queue, capacity, takers, offerers) if queue.nonEmpty =>
              val (i, rest) = queue.dequeue
              val ((move, release), tail) = offerers.dequeue
              StateBound(rest.enqueue(move), capacity, takers, tail) -> release.complete(()).as(i)
            case StateBound(queue, capacity, takers, offerers) if offerers.nonEmpty =>
              val ((i, release), rest) = offerers.dequeue
              StateBound(queue, capacity, takers, rest) -> release.complete(()).as(i)
            case StateBound(queue, capacity, takers, offerers) =>
              val cleanup: F[Unit] = stateR.update { s => s.copy(takers = s.takers.filter(_ ne taker)) }
              StateBound(queue, capacity, takers.enqueue(taker), offerers) -> poll(taker.get).onCancel(cleanup)
          }.flatten
        }
      }

    for {
      i <- take
      _ <- if(i % 10000 == 0) Console[F].println(s"Consumer $id has reached $i items") else Async[F].unit
      _ <- consumerDeferBoundCancelable(id, stateR)
    } yield ()

  }

}


case class State[F[_], A](queue: Queue[A], takers: Queue[Deferred[F, A]])

object State {
  def empty[F[_], A]: State[F, A] = State(Queue.empty, Queue.empty)
}

case class StateBound[F[_], A](queue: Queue[A],
                               capacity: Int,
                               takers: Queue[Deferred[F, A]],
                               offers: Queue[(A, Deferred[F, Unit])])


object StateBound {
  def empty[F[_], A](capacity: Int): StateBound[F, A] = StateBound(Queue.empty, capacity, Queue.empty, Queue.empty)
}


























































































