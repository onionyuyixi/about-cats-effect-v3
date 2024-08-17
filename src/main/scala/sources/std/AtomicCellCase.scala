package sources.std;
import cats.effect.IO
import cats.effect.std.AtomicCell
import cats.effect.unsafe.implicits.global
import cats.kernel.Monoid
object AtomicCellCase extends App {

  case class IntVal(num: Int) extends AnyVal

  implicit val intValMonoid: Monoid[IntVal] = new Monoid[IntVal] {
    override def empty: IntVal = IntVal(0)

    override def combine(x: IntVal, y: IntVal): IntVal = IntVal(x.num + y.num)
  }

  val init = IntVal(100)
  val result = AtomicCell[IO]
    .of(init)
    .flatMap(num => IO.unit >> num.update(a => IntVal(a.num + 100)) >> num.get)
    .unsafeRunSync()

  println(s"init pluses 100  is $result")


}