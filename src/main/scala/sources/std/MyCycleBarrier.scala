package sources.std

import cats.effect.implicits.monadCancelOps_
import cats.effect.kernel.{Deferred, GenConcurrent}
import cats.~>

abstract class MyCycleBarrier[F[_]] {
  self =>

  def await: F[Unit]

  def mapK[G[_]](f: F ~> G) = new MyCycleBarrier[G] {
    override def await: G[Unit] = f(self.await)
  }

}

object MyCycleBarrier {

  import cats.syntax.all._

  def apply[F[_]](capacity: Int)(implicit F: GenConcurrent[F, ?]): F[MyCycleBarrier[F]] = {
    if (capacity < 1)
      throw new IllegalArgumentException("CycleBarrier initial parameter must be greater than 1")

    case class State(awaiting: Int, epoch: Int, block: Deferred[F, Unit])

    F.deferred[Unit].map((block: Deferred[F, Unit]) => State(capacity, 0, block))
      .flatMap((state: State) => F.ref(state))
      .map { stateRef =>
        new MyCycleBarrier[F] {
          override def await: F[Unit] =
            F.deferred[Unit].flatMap { newBlock =>
              stateRef.flatModifyFull {
                case (poll, State(awaiting, epoch, block)) =>
                  val newAwaiting = awaiting - 1
                  if (newAwaiting == 0) // 所有任务都到达了同一地点 可以放行 block结束完成
                    State(capacity, epoch + 1, newBlock) -> block.complete(()).void
                  else {
                    // 尚有任务还未到站 减少一个等待数量
                    val newState = State(newAwaiting, epoch, block)
                    val cleanUp = stateRef.update { s =>
                      if (s.epoch == epoch) s.copy(awaiting = awaiting + 1)
                      else s
                    }
                    newState -> poll(block.get).onCancel(cleanUp)
                  }
              }
            }
        }

      }

  }
}