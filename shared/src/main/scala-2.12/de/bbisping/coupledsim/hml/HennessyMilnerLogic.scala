package de.bbisping.coupledsim.hml

object HennessyMilnerLogic {

  abstract sealed class Formula[A]

  case class And[A](subterms: List[Formula[A]]) extends Formula[A] {
    override def toString = {
      if (subterms.isEmpty) {
        "⊤"
      } else {
        subterms.mkString("⋀{", ",", "}")
      }
    }
  }

  case class Observe[A](action: A, andThen: Formula[A]) extends Formula[A] {
    override def toString = "⟨" + action.toString + "⟩" + andThen.toString
  }

  case class Negate[A](andThen: Formula[A]) extends Formula[A] {
    override def toString = "¬" + andThen.toString
  }

}