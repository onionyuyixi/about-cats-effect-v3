
package sources.cases

import cats.data.State
import cats.free.Free
import cats.~>

sealed trait KVStore[A]

case class Put[T](key: String, value: T) extends KVStore[Unit]

case class Get[T](key: String) extends KVStore[Option[T]]

case class Delete[T](key: String) extends KVStore[Unit]


case object KVStoreApi extends App {

  type FreeKVStrore[A] = Free[KVStore, A]

  def put[T](k: String, v: T): FreeKVStrore[Unit] = Free.liftF[KVStore, Unit](Put(k, v))

  def get[T](k: String): FreeKVStrore[Option[T]] = Free.liftF[KVStore, Option[T]](Get(k))

  def delete(k: String): FreeKVStrore[Unit] = Free.liftF[KVStore, Unit](Delete(k))

  def update[T](key: String, f: T => T): FreeKVStrore[Unit] =
    for {
      vMaybe <- get[T](key)
      _ <- vMaybe.map(v => put(key, f(v))).getOrElse(Free.pure())
    } yield ()

  def program: FreeKVStrore[Option[Int]] =
    for {
      _ <- put("wild-cats", 2)
      _ <- update[Int]("wild-cats", _ + 12)
      _ <- put("tame-cats", 5)
      n <- get[Int]("wild-cats")
      _ <- delete("tame-cats")
    } yield n


  //  def impureCompiler: KVStore ~> Id = new(KVStore ~> Id) {
  //
  //    val kvs = mutable.Map.empty[String, Any]
  //
  //    override def apply[A](fa: KVStore[A]): Id[A] =
  //      fa match {
  //        case Put(k, v) =>
  //          println(s"put($k, $v)")
  //          kvs(k) = v
  //          ()
  //        case Get(key) =>
  //          println(s"get($key)")
  //          kvs.get(key)
  //        case Delete(key) =>
  //          println(s"delete($key)")
  //          kvs.remove(key)
  //          ()
  //      }
  //  }
  //
  //
  //  println(program.foldMap(impureCompiler))

  private type KVStoreState[A] = State[Map[String, Any], A]

  private val pureCompiler = new(KVStore ~> KVStoreState) {
    override def apply[A](fa: KVStore[A]): KVStoreState[A] =
      fa match {
        case Put(k, v) => State.modify(_.updated(k, v))
        case Get(key) => State.inspect(_.get(key))
        case Delete(key) => State.modify(_.-(key))
      }
  }

  println(program)
  println(program.step)

  val value: KVStoreState[Option[Int]] = program.foldMap(pureCompiler)
  println(value.run(Map.empty).value)


}


