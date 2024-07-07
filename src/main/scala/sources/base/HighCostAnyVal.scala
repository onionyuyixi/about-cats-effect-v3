package sources.base
// https://failex.blogspot.com/2017/04/the-high-cost-of-anyval-subclasses.html#markdown-header-what-can-you-do-with-a-box-what-can-you-do-without-a-box
case class Label(str: String) extends AnyVal

object Label {
  def apply(s: String): Label =
    new Label(s)
}

class MyFirstTests {

  def combineLabels(l: Label, r:Label): Label =
    Label(l.str + r.str)

  def printLabels(): Unit = {
    val fst: Label = Label("hello")
    val snd: Label = Label("world")
    println(fst)
    println(snd)
    val lbls = List(fst, snd)
    lbls.map{x => Label(x.str + "Aux")}
    (fst, snd)
  }

}


case class Labelled[A](lbl: Label, a: A)


