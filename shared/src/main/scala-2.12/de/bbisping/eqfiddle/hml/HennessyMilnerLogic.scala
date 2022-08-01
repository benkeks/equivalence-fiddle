package de.bbisping.eqfiddle.hml

object HennessyMilnerLogic {


  abstract class Formula[A] {

    def isPositive: Boolean

    def isImmediate: Boolean

  }

  case class And[A](subterms: Set[Formula[A]]) extends Formula[A] {

    override def toString = {
      if (subterms.isEmpty) {
        "⊤"
      } else {
        subterms.mkString("⋀{", ",", "}")
      }
    }
    override val isPositive = true

    override val isImmediate = subterms.nonEmpty
  }

  def True[A]: And[A] = And[A](Set())

  case class Observe[A](action: A, andThen: Formula[A]) extends Formula[A] {

    override def toString = "⟨" + action.toString + "⟩" + andThen.toString

    override val isPositive = true

    override val isImmediate = true
  }

  case class Pass[A](andThen: Formula[A]) extends Formula[A] {

    override def toString = andThen match {
      case And(subterms) => subterms.mkString("⨇{", ",", "}")
      case Observe(action, andThen) => "⟪" + action.toString + "⟫" + andThen.toString
      case _ => "ϵ" + andThen.toString
    }

    override val isPositive = andThen.isPositive

    override val isImmediate = false
  }

  case class Negate[A](andThen: Formula[A]) extends Formula[A] {

    override def toString = "¬" + andThen.toString

    override val isPositive = false//!andThen.isPositive

    override val isImmediate = andThen.isImmediate

  }
}