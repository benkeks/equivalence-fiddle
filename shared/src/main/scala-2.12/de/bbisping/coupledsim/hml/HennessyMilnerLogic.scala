package de.bbisping.coupledsim.hml

object HennessyMilnerLogic {

  abstract sealed class Formula[A] {


    /** the maximal depth of the subformulas (⊤ has height 0, negation are neutral wrt. height) */
    def height: Int

    /** the maximal amount of negations when descending into this formula */
    def negationLevels: Int

    /** the maximal height of negative subformulas */
    def highestNegation: Int

    /** the maximal amount of conjunctions when descending into this formula */
    def conjunctionLevels: Int

    /** the maximal height of conjunctive subformulas */
    def highestConjunction: Int

    /** whether there are conjunctions with not only negations as direct subformulas */
    def mixedConjunctions: Boolean

    /** names the coarsest notion of equivalence where this formula is part of the distinguishing formulas */
    def classifyFormula(): String = {
      if (negationLevels == 0) {
        if (highestConjunction == 0) {
          "trace"
        } else {
          "simulation"
        } 
      } else if (negationLevels == 1 && conjunctionLevels == 1) {
        if (highestNegation == 1 && highestConjunction <= 1) {
          if (mixedConjunctions) {
            "ready"
          } else {
            "failure"
          }
        } else if (highestNegation == 1 && highestConjunction > 1) {
          //TODO: Ensure that exactly one sub formula at conjunctions may be deep (otherwise this definition is wrong
          if (mixedConjunctions) {
            "~ready-trace"
          } else {
            "~failure-trace"
          }
        } else if (highestNegation <= highestConjunction) {
          if (mixedConjunctions) {
            "possible-futures"
          } else {
            "impossible-futures"
          }
        } else {
          "1-nested-simulation"
        }
      } else {
        if (negationLevels == 1 && highestNegation <= 1) {
          "ready-simulation"
        } else {
          negationLevels + "-nested-simulation"
        }
      }
    }

  }

  case class And[A](subterms: List[Formula[A]]) extends Formula[A] {
    override def toString = {
      if (subterms.isEmpty) {
        "⊤"
      } else {
        subterms.mkString("⋀{", ",", "}")
      }
    }

    override val height: Int = if (subterms.nonEmpty) {
      subterms.map(_.height).max + 1
    } else {
      0
    }

    override val negationLevels: Int = if (subterms.nonEmpty) {
      subterms.map(_.negationLevels).max
    } else {
      0
    }

    override val highestNegation: Int = if (subterms.nonEmpty) {
      subterms.map(_.highestNegation).max
    } else {
      -1
    }

    override val conjunctionLevels: Int = if (subterms.nonEmpty) {
      subterms.map(_.conjunctionLevels).max + 1
    } else {
      0
    }

    override val highestConjunction: Int = height

    override val mixedConjunctions: Boolean = subterms.exists {
      f => !f.isInstanceOf[Negate[_]] || f.mixedConjunctions
    }

  }

  case class Observe[A](action: A, andThen: Formula[A]) extends Formula[A] {
    override def toString = "⟨" + action.toString + "⟩" + andThen.toString

    override val height: Int = andThen.height + 1
    
    override val negationLevels: Int = andThen.negationLevels

    override val highestNegation: Int = andThen.highestNegation

    override val conjunctionLevels: Int = andThen.conjunctionLevels

    override val highestConjunction: Int = andThen.highestConjunction

    override val mixedConjunctions: Boolean = andThen.mixedConjunctions
  }

  case class Negate[A](andThen: Formula[A]) extends Formula[A] {
    override def toString = "¬" + andThen.toString

    override val height: Int = andThen.height

    override val negationLevels: Int = andThen.negationLevels + 1

    override val highestNegation: Int = height

    override val conjunctionLevels: Int = andThen.conjunctionLevels

    override val highestConjunction: Int = andThen.highestConjunction
    
    override val mixedConjunctions: Boolean = andThen.mixedConjunctions
  }

}