package sources.cats_.effect

import cats.{Functor, data}
import cats.effect._
import cats.implicits._

import java.util.concurrent.ConcurrentHashMap
import java.util.concurrent.atomic.AtomicBoolean

// 主要利用了ref来修改map
trait MapRef[F[_], K, V] extends (K => Ref[F, V]) {

  def apply(k: K): Ref[F, V]

}

object MapRef extends MapRefCompanionPlatform {

  def ofShardedImmutableMap[F[_] : Concurrent, K, V](shardCount: Int): F[MapRef[F, K, Option[V]]] = {
    assert(shardCount >= 1, "MapRef.sharded should have at least 1 shard")
    val emptyMapRef: F[Ref[F, Map[K, V]]] = Concurrent[F].ref[Map[K, V]](Map.empty)
    List
      .fill(shardCount)(())
      .traverse(_ => emptyMapRef)
      .map(fromSeqRefs(_))
  }

  def fromSeqRefs[F[_] : Concurrent, K, V](seq: Seq[Ref[F, Map[K, V]]]): MapRef[F, K, Option[V]] = {
    val size = seq.size
    val array = seq.toArray
    val refFunction = (k: K) => {
      val location = Math.abs(k.## % size)
      array(location)
    }
    new MapRef[F, K, Option[V]] {
      override def apply(k: K): Ref[F, Option[V]] =
        fromSingleImmutableMapRef(refFunction(k)).apply(k)
    }
  }

  def fromSingleImmutableMapRef[F[_] : Functor, K, V](ref: Ref[F, Map[K, V]]): MapRef[F, K, Option[V]] = {
    new MapRef[F, K, Option[V]] {
      override def apply(k: K): Ref[F, Option[V]] = {
        val func: (Map[K, V] => Option[V], Map[K, V] => Option[V] => Map[K, V]) => Ref[F, Option[V]] = Ref.lens(ref)
        func(map => map.get(k), map => (opt: Option[V]) => opt.fold(map - k)(v => map + (k -> v)))
      }
    }
  }

  def ofSingleImmutableMap[F[_] : Concurrent, K, V](
                                                     map: Map[K, V] = Map.empty[K, V]): F[MapRef[F, K, Option[V]]] =
    Concurrent[F].ref(map).map(fromSingleImmutableMapRef[F, K, V](_))


  private class ConcurrentHashMapImpl[F[_], K, V](chm: ConcurrentHashMap[K, V], sync: Sync[F])
    extends MapRef[F, K, Option[V]] {

    private implicit def syncF: Sync[F] = sync

    private val fnone0: F[None.type] = syncF.pure(None)

    def fnone[A]: F[Option[A]] = fnone0.widen[Option[A]]

    def delay[A](a: => A): F[A] = sync.delay(a)

    class HandleRef(k: K) extends Ref[F, Option[V]] {

      override def access: F[(Option[V], Option[V] => F[Boolean])] = delay {
        val hasBeenCalled = new AtomicBoolean(false)
        val init = chm.get(k)
        if (init == null) {
          val set: Option[V] => F[Boolean] = {
            case None =>
              delay(hasBeenCalled.compareAndSet(false, true) && !chm.contains(k))
            case Some(newV) =>
              delay(hasBeenCalled.compareAndSet(false, true) && chm.putIfAbsent(k, newV) == null)
          }
          (None, set)
        } else {
          val set: Option[V] => F[Boolean] = {
            case None =>
              delay(hasBeenCalled.compareAndSet(false, true) && chm.remove(k, init))
            case Some(newV) =>
              delay(hasBeenCalled.compareAndSet(false, true) && chm.replace(k, init, newV))
          }
          (Some(init), set)
        }
      }

      override def getAndSet(a: Option[V]): F[Option[V]] =
        a match {
          case None =>
            delay(Option(chm.remove(k)))
          case Some(v) =>
            delay(Option(chm.put(k, v)))
        }

      override def modify[B](f: Option[V] => (Option[V], B)): F[B] = {
        def loop: F[B] = tryModify(f).flatMap {
          case None => loop
          case Some(value) => sync.pure(value)
        }

        loop
      }

      override def modifyState[B](state: data.State[Option[V], B]): F[B] =
        modify(state.run(_).value)

      override def tryUpdate(f: Option[V] => Option[V]): F[Boolean] =
        tryModify { opt => (f(opt), ()) }.map(_.isDefined)

      override def tryModify[B](f: Option[V] => (Option[V], B)): F[Option[B]] =
        delay[F[Option[B]]] {
          val init = chm.get(k)
          if (init == null) {
            f(None) match {
              case (None, b) =>
                sync.pure(b.some)
              case (Some(newV), b) =>
                if (chm.putIfAbsent(k, newV) == null) sync.pure(b.some)
                else fnone
            }
          } else {
            f(Some(init)) match {
              case (None, b) =>
                if (chm.remove(k, init)) sync.pure(Some(b))
                else fnone[B]
              case (Some(next), b) =>
                if (chm.replace(k, init, next)) sync.pure(Some(b))
                else fnone[B]
            }
          }
        }.flatten

      override def update(f: Option[V] => Option[V]): F[Unit] = {
        def loop: F[Unit] = tryUpdate(f).flatMap {
          case true => sync.unit
          case false => loop
        }

        loop
      }

      override def tryModifyState[B](state: data.State[Option[V], B]): F[Option[B]] =
        tryModify(state.run(_).value)

      override def set(a: Option[V]): F[Unit] =
        a match {
          case None => delay {
            chm.remove(k);
            ()
          }
          case Some(v) => delay {
            chm.put(k, v);
            ()
          }
        }

      override def get: F[Option[V]] =
        delay(Option(chm.get(k)))
    }

    override def apply(k: K): Ref[F, Option[V]] = new HandleRef(k)
  }

  def fromConcurrentHashMap[F[_]: Sync, K, V](map: ConcurrentHashMap[K, V]): MapRef[F, K, Option[V]] =
    new ConcurrentHashMapImpl[F, K, V](map, Sync[F])

  def fromScalaConcurrentMap[F[_]: Sync, K, V](
                                                map: scala.collection.concurrent.Map[K, V]): MapRef[F, K, Option[V]] =
    new ScalaConcurrentMapImpl[F, K, V](map)

  private class ScalaConcurrentMapImpl[F[_], K, V](map: scala.collection.concurrent.Map[K, V])(
    implicit sync: Sync[F])
    extends MapRef[F, K, Option[V]] {

    val fnone0: F[None.type] = sync.pure(None)
    def fnone[A]: F[Option[A]] = fnone0.widen[Option[A]]

    class HandleRef(k: K) extends Ref[F, Option[V]] {
      def access: F[(Option[V], Option[V] => F[Boolean])] =
        sync.delay {
          val hasBeenCalled = new AtomicBoolean(false)
          val init = map.get(k)
          init match {
            case None =>
              val set: Option[V] => F[Boolean] = { (opt: Option[V]) =>
                opt match {
                  case None =>
                    sync.delay(hasBeenCalled.compareAndSet(false, true) && !map.contains(k))
                  case Some(newV) =>
                    sync.delay {
                      // it was initially empty
                      hasBeenCalled
                        .compareAndSet(false, true) && map.putIfAbsent(k, newV).isEmpty
                    }
                }
              }
              (None, set)
            case Some(old) =>
              val set: Option[V] => F[Boolean] = { (opt: Option[V]) =>
                opt match {
                  case None =>
                    sync.delay(hasBeenCalled.compareAndSet(false, true) && map.remove(k, old))
                  case Some(newV) =>
                    sync.delay(
                      hasBeenCalled.compareAndSet(false, true) && map.replace(k, old, newV))
                }
              }
              (init, set)
          }
        }

      def get: F[Option[V]] =
        sync.delay(map.get(k))

      override def getAndSet(a: Option[V]): F[Option[V]] =
        a match {
          case None =>
            sync.delay(map.remove(k))
          case Some(v) =>
            sync.delay(map.put(k, v))
        }

      def modify[B](f: Option[V] => (Option[V], B)): F[B] = {
        def loop: F[B] = tryModify(f).flatMap {
          case None => loop
          case Some(b) => sync.pure(b)
        }
        loop
      }

      def modifyState[B](state: data.State[Option[V], B]): F[B] =
        modify(state.run(_).value)

      def set(a: Option[V]): F[Unit] =
        a match {
          case None => sync.delay { map.remove(k); () }
          case Some(v) => sync.delay { map.put(k, v); () }
        }

      def tryModify[B](
                        f: Option[V] => (Option[V], B))
      : F[Option[B]] = // we need the suspend because we do effects inside
        sync.delay {
          val init = map.get(k)
          init match {
            case None =>
              f(None) match {
                case (None, b) =>
                  // no-op
                  sync.pure(b.some)
                case (Some(newV), b) =>
                  sync.delay(map.putIfAbsent(k, newV).fold[Option[B]](b.some)(_ => None))
              }
            case Some(initV) =>
              f(init) match {
                case (None, b) =>
                  if (map.remove(k, initV)) sync.pure(b.some)
                  else fnone[B]
                case (Some(next), b) =>
                  if (map.replace(k, initV, next)) sync.pure(b.some)
                  else fnone[B]
              }
          }
        }.flatten

      def tryModifyState[B](state: data.State[Option[V], B]): F[Option[B]] =
        tryModify(state.run(_).value)

      def tryUpdate(f: Option[V] => Option[V]): F[Boolean] =
        tryModify { opt => (f(opt), ()) }.map(_.isDefined)

      def update(f: Option[V] => Option[V]): F[Unit] = {
        def loop: F[Unit] = tryUpdate(f).flatMap {
          case true => sync.unit
          case false => loop
        }
        loop
      }
    }

    /**
     * Access the reference for this Key
     */
    def apply(k: K): Ref[F, Option[V]] = new HandleRef(k)

  }
}


private trait MapRefCompanionPlatform {

  def inScalaConcurrentTrieMap[G[_] : Sync, F[_] : Sync, K, V]: G[MapRef[F, K, Option[V]]] =
    Sync[G]
      .delay(scala.collection.concurrent.TrieMap.empty[K, V])
      .map(MapRef.fromScalaConcurrentMap[F, K, V](_))

  def ofScalaConcurrentTrieMap[F[_] : Sync, K, V]: F[MapRef[F, K, Option[V]]] =
    Sync[F]
      .delay(scala.collection.concurrent.TrieMap.empty[K, V])
      .map(MapRef.fromScalaConcurrentMap[F, K, V](_))

}