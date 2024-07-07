package sources.base

object Labels {

  sealed abstract class LabelImpl {
    type T

    def apply(s: String): T

    def unwrap(lbl: T): String
  }

  // do not forget `: LabelImpl`; it is key
  val labelString: LabelImpl = new LabelImpl {
    type T = String

    override def apply(s: String): String = s

    override def unwrap(lbl: T): String = lbl
  }

  type LabelStr = labelString.T


  val fst = Label("hello")
  val snd = Label("world")




}
