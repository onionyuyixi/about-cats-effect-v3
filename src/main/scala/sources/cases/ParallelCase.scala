package sources.cases

import cats.Semigroupal
import cats.data.NonEmptyList

import java.time.LocalDateTime

object ParallelCase extends App {

  import cats.syntax.all._


  case class Name(value: String)

  case class Age(value: Int)

  case class Person(name: Name, age: Age)


  def parse(s: String): Either[NonEmptyList[String], Int] =
    if (s.matches("-?[0-9]")) Right(s.toInt)
    else Left(NonEmptyList.one(s" $s is not a valid Int"))

  def validateAge(a: Int): Either[NonEmptyList[String], Age] =
    if (a > 18) Right(Age(a))
    else Left(NonEmptyList.one(s" $a is not old enough "))

  def validateName(n: String): Either[NonEmptyList[String], Name] = {
    if (n.length >= 8) Right(Name(n))
    else Left(NonEmptyList.one(s"$n Does not have enough characters"))
  }

  def parsePerson(ageString: String, nameString: String): Either[NonEmptyList[String], Person] =
    for {
      age <- parse(ageString)
      person <- (validateName(nameString).toValidated, validateAge(age).toValidated)
        .mapN(Person)
        .toEither
    } yield person

  private val l: Long = System.currentTimeMillis()
  for (i <- 0 until 1 * 10000) {
    parsePerson("18", "onionyuyixi")
  }

  println("seq--" + (System.currentTimeMillis() - l))

  def parsePerson1(ageString: String, nameString: String): Either[NonEmptyList[String], Person] =
    for {
      age <- parse(ageString)
      person <- (validateName(nameString), validateAge(age)).parMapN(Person)
    } yield person

  private val l1: Long = System.currentTimeMillis()
  for (i <- 0 until 1 * 10000) {
    parsePerson1("18", "onionyuyixi")
  }
  println("para---" + (System.currentTimeMillis() - l1))

  private val sequence: Either[NonEmptyList[String], List[Int]] =
    List(
      Right(42),
      Left(NonEmptyList.one("Error 1")),
      Left(NonEmptyList.one("Error 2"))
    ).parSequence

  println(sequence.isLeft)
  println(sequence)


  private val ints: List[Int] = (List(1, 2, 3), List(4, 5, 6)).mapN(_ + _)
  println(ints)

  private val ints1: List[Int] = (List(1, 2, 3), List(4, 5, 6)).parMapN(_ + _)
  println(ints1)


  private val l2: Long = System.currentTimeMillis()
  for (i <- 0 until 1 * 1000) {
    (List.range(0, 1000), List.range(0, 1000)).mapN(_ + _)
  }
  println("seq---" + LocalDateTime.now() + " ////////// " + (System.currentTimeMillis() - l2))

  private val l3: Long = System.currentTimeMillis()
  for (i <- 0 until 200 * 1000) {
    (List.range(0, 1000), List.range(0, 1000)).parMapN(_ + _)
  }
  println("par---" + LocalDateTime.now() + " ////////// " + (System.currentTimeMillis() - l3))


}

object CatsWithScalaParallel extends App {

  import cats.syntax.all._

  type ErrorOr[A] = Either[Vector[String], A]
  val error1: ErrorOr[Int] = Left(Vector("Error 1"))
  val error2: ErrorOr[Int] = Left(Vector("Error 2"))


  val result: ErrorOr[(Int, Int)] = Semigroupal[ErrorOr].product(error1, error2)

  val tupled: ErrorOr[(Int, Int)] = (error1, error2).tupled

  val tupled2: ErrorOr[(Int, Int)] = (error1, error2).parTupled

  type ErrorOrList[A] = Either[List[String], A]
  val errStr1: ErrorOrList[Int] = Left(List("error 1"))
  val errStr2: ErrorOrList[Int] = Left(List("error 2"))
  val tupled1: ErrorOrList[(Int, Int)] = (errStr1, errStr2).parTupled
  println(tupled1)




}
