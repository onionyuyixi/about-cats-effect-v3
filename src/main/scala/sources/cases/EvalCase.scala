package sources.cases

import cats.Eval

object EvalCase extends App {

  val eagar: Eval[Int] = Eval.now {
    println("Running expensive evaluation")
    1 + 2 * 3
  }

  println(eagar)

  println(eagar.value)


  val lazyEval = Eval.later {
    println("Running expensive calculation...")
    1 + 2 * 3
  }
  println(lazyEval)
  println(lazyEval.value)
  println(lazyEval.value)

  val always = Eval.always {
    println("Running expensive calculation...")
    1 + 2 * 3
  }

  println(always.value)
  println(always.value)


  def even(value: Int): Eval[Boolean] =
    Eval.always(0 == value).flatMap {
      case true => Eval.True
      case false =>
        odd(value - 1)
    }

  def odd(n: Int): Eval[Boolean] =
    Eval.always(n == 0).flatMap {
      case true => Eval.False
      case false =>
        even(n - 1)
    }

  println(odd(1000000).value)

  println(Eval.defer(eagar).value)
  println(Eval.defer(lazyEval).value)
  println(Eval.defer(always).value)


}
