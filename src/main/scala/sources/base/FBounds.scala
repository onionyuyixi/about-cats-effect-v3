package sources.base

import shapeless.HList.ListCompat.::
import shapeless.{HList, HNil, Poly1}
import sources.base.ForCollection.{Fish, Kitty, Pet, esquire}
import sources.base.Shapeless.polyEsq

import java.awt.Color
import scala.::
import scala.language.existentials
// https://tpolecat.github.io/2015/04/29/f-bounds.html

object FBounds extends App {

  trait Pet {
    def name: String

    // 定义返回Pet 其范围太模糊 容易错乱 not accurate
    // 比如有子类 Dog Cat
    // 则其实现方法可以故意互相混淆 dog误认cat cat错看dog
    def renamed(newName: String): Pet
  }

  case class Fish(name: String, age: Int) extends Pet {
    def renamed(newName: String): Fish = copy(name = newName)

  }

  // 这里的问题是 方法定义是支持抽象且covariant
  // 而实现的时候 实际上是确定的类型   compile error
  // def esquire[A <: Pet](a: A): A = a.renamed(a.name + ", Esq.")

  case class Kitty(name: String, color: Color) extends Pet {
    // 这里应当返回Kitty才准确
    // 出现的问题就是 trait中的rename方法 在返回结果的时候 限制不准
    def renamed(newName: String): Fish = Fish(newName, 42) // oops
  }

  // An F-bounded type is parameterized over its own subtypes,
  // which allows us to “pass” the implementing type as an argument to the superclass.
  // 这里可以分步骤来理解
  // 首先支持param type 即FPet[A]
  // 其次 A 必须是FPet的实现 即采用covariant A<:FPet[A]
  trait FPet[A <: FPet[A]] {
    def name: String

    def renamed(newName: String): A
  }

  case class FFish(name: String, age: Int) extends FPet[FFish] {
    def renamed(newName: String): FFish = copy(name = newName)

  }

  def esquireF[A <: FPet[A]](a: A): A = a.renamed(a.name + ", Esq.")

  case class FKitty(name: String, color: Color) extends FPet[FKitty] {
    // 如果还是返回FFish 此时编译不过 问题就是不符合param type definition
    // def renamed(newName: String): FFish =FFish(newName, 42)
    override def renamed(newName: String): FKitty = FKitty(newName, Color.CYAN)
  }


  class Mammal(val name: String) extends FPet[Mammal] {
    def renamed(newName: String) = new Mammal(newName)
  }

  case class Monkey(override val name: String) extends Mammal(name) {
    override def renamed(newName: String): Monkey = Monkey(newName)
  }

  // sources.base.FBounds$Mammal 结果Monkey 成了Mammal
  // 这里的type param 无法自定义 只能重袭父类
  // 好在可以补救 重写父类方法  不过这样不满足我们的方便
  println(Monkey("  Monkey ").renamed("Monkey no Mammal").getClass.getName)


  // 以上二途 均以多态(subtype)为害  一者以其疏略之阔  一者以其逼仄之隘 不若弃之远以造他法 另创胜境
  // type class 犹如盘古 手斥巨斧 扫荡烟障而天地生 震动混沌而万物玩
  // 其依穆噗李斯忒（implicit）可谓勇猛刚劲 能与巨斧轩轾角力也
  // 又如钢珠走盘 其明快不涩 止于可止 行于当行 与其关腱处 游刃有余
  // 暗处着力而不显山露水 可谓蕴涵深厚
  // 如画中之山 若见其高崇 须云盘在腰 而松木隐隐斜依 不可尽现

  // how about typeclass
  // This is a general strategy. By identifying methods that require us to return the “current” type and
  // moving them to a typeclass we can guarantee that our desired constraint is met
  trait TPet {
    def name: String
  }

  trait Rename[A] {
    // return the “current” type and moving them to a typeclass
    def rename(a: A, newName: String): A
  }

  case class TFish(name: String, age: Int) extends TPet

  object TFish {
    // 提供implicit
    implicit val FishRename: Rename[TFish] = new Rename[TFish] {
      override def rename(a: TFish, newName: String): TFish =
        a.copy(name = newName)
    }
  }

  // 使用implicit
  implicit class RenameOps[A](a: A)(implicit rename: Rename[A]) {
    def rename(newName: String): A = rename.rename(a, newName)
  }

  private val jack: TFish = TFish("JACK", 12)
  println(jack)
  private val tom: TFish = jack.rename("Tom")
  println(tom)


  // 此时限定A的type scope 实际上这跟上面的 RenameOps差不多的用意
  // A <: TPet : Rename  的意思是 A是TPet的实现类 但同时作为Rename的type param
  def esquire[A <: TPet : Rename](a: A): A = a.rename(a.name + ", Esq.")

  println(esquire(jack))


  // How about only a Typeclass?
  // There is an informal conjecture that ad-hoc and
  // parametric polymorphism are really all we need in a programming language;
  // we can get along just fine without subtyping.
  // 不采用subtyping照样轻松搞定
  trait OTPet[A] {
    def name(a: A): String

    def renamed(a: A, newName: String): A
  }

  implicit class OTPetOps[A](a: A)(implicit otp: OTPet[A]) {
    def rename(newName: String): A = otp.renamed(a, newName)
  }

  case class OTFish(name: String, age: Int)

  object OTFish {
    implicit val OTFishRename: OTPet[OTFish] = new OTPet[OTFish] {
      override def name(a: OTFish): String = a.name

      override def renamed(a: OTFish, newName: String): OTFish = a.copy(name = newName)
    }
  }

  println(OTFish("Susan", 20).rename("Arithesz"))


}


object ForCollection extends App {

  trait Pet[A] {
    def name(a: A): String

    def renamed(a: A, newName: String): A
  }

  implicit class PetOps[A](a: A)(implicit ev: Pet[A]) {
    def name: String = ev.name(a)

    def renamed(newName: String): A = ev.renamed(a, newName)
  }

  case class Fish(name: String, age: Int)

  object Fish {
    implicit object FishPet extends Pet[Fish] {
      def name(a: Fish): String = a.name

      def renamed(a: Fish, newName: String): Fish = a.copy(name = newName)
    }
  }

  case class Kitty(name: String, color: Color)

  object Kitty {
    implicit object KittyPet extends Pet[Kitty] {
      def name(a: Kitty): String = a.name

      def renamed(a: Kitty, newName: String): Kitty = a.copy(name = newName)
    }
  }

  def esquire[A: Pet](a: A): A = a.renamed(a.name + ", Esq.")


  val bob = Fish("Bob", 12)
  val thor = Kitty("Thor", Color.ORANGE)


  val pets = List[(A, Pet[A]) forSome {type A}]((bob, implicitly[Pet[Fish]]), (thor, implicitly[Pet[Kitty]]))

  println(pets.map { case (a, pa) => esquire(a)(pa) })


}


object TypeMember extends App {

  trait TM[F[_]] {

    // type member
    type A

    val a: A

    val fa: F[A]

  }


  private object TM {
    // 此处得力于implicit 来确定了F[_]
    def apply[F[_], A0](a0: A0)(implicit ev: F[A0]): TM[F] = new TM[F] {
      override type A = A0
      override val a: A = a0
      override val fa: F[A] = ev
    }
  }

  val bob = Fish("Bob", 12)
  val thor = Kitty("Thor", Color.ORANGE)


  private val tmBob: TM[Pet] = TM(bob)
  private val tmThor: TM[Pet] = TM(thor)

  val pets = List(tmBob, tmThor)

  println(pets.map { p => esquire(p.a)(p.fa) })


}

object Shapeless extends App {

  object polyEsq extends Poly1 {
    implicit def default[A: Pet]: polyEsq.Case.Aux[A, A] = at[A](esquire(_))
  }

  val bob = Fish("Bob", 12)
  val thor = Kitty("Thor", Color.ORANGE)

  println((bob :: thor :: HNil) map polyEsq)






}