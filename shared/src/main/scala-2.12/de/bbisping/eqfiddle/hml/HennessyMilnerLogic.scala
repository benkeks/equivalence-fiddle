package de.bbisping.eqfiddle.hml

object HennessyMilnerLogic {


  def getLeastDistinguishing[A](formulas: Set[Formula[A]]): Set[Formula[A]] = {
    val classifications = formulas.map(f => (f, f.classifyFormula()))
    val allClassBounds = classifications.flatMap(_._2._2.map(_._2))

    for {
      (f, (_, namedClassBounds)) <- classifications
      classBounds = namedClassBounds.map(_._2)
      // just keep formulas where one of the classifications is dominated by no other classification
      if classBounds.exists(classBound => !allClassBounds.exists(_ strictlyBelow classBound))
    } yield f
  }

  abstract sealed class Formula[A] {

    def obsClass: ObservationClass

    def isPositive: Boolean

    /** class of this formula if it appears at the top level */
    def getRootClass() = {
      ObservationClass(
        obsClass.observationHeight,
        obsClass.conjunctionLevels + (if (!isPositive) 1 else 0),
        obsClass.maxPositiveDeepBranches,
        obsClass.maxPositiveBranches,
        obsClass.negationLevels,
        obsClass.maxNegatedHeight
      )
    }

    /** names the coarsest notion of equivalence where this formula is part of the distinguishing formulas */
    def classifyFormula(): (ObservationClass, List[(String, ObservationClass)]) = {
      val balancedClass = getRootClass()
      val classifications = ObservationClass.LTBTS.collect { case (name, cl) if (balancedClass lub cl) == cl => (name, cl) }
      var currentMax = List[ObservationClass]()
      val leastClassifications = for {
        (name, cl) <- classifications
        if !currentMax.exists(cl.above(_))
      } yield {
        currentMax = cl :: currentMax
        (name, cl)
      }
      (balancedClass, leastClassifications)
    }

    def classifyNicely() = {
      val (_, classifications) = classifyFormula()
      classifications.map(_._1).mkString(",")
    }

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

    override val obsClass = {
      
      if (subterms.isEmpty) {
        ObservationClass(0,0,0,0,0,0)
      } else {
        val positiveSubterms = subterms.filter(_.isPositive)
        val positiveFlatCount = positiveSubterms.count(_.obsClass.observationHeight <= 1)

        ObservationClass(
          observationHeight = subterms.map(_.obsClass.observationHeight).max,
          /** the maximal amount of conjunctions when descending into a formula */
          conjunctionLevels = subterms.map(_.obsClass.conjunctionLevels).max + 1,
          /** the maximal amount of positive deep branches (observationHeight > 1)*/
          maxPositiveDeepBranches = (subterms.map(_.obsClass.maxPositiveDeepBranches) + (positiveSubterms.size - positiveFlatCount)).max,
          /** the maximal amount of positive branches */
          maxPositiveBranches = (subterms.map(_.obsClass.maxPositiveBranches) + positiveSubterms.size).max,
          /** the maximal amount of negations when descending into a formula */
          negationLevels = subterms.map(_.obsClass.negationLevels).max,
          /** maximal observationHeight of negative subformulas */
          maxNegatedHeight = subterms.map(_.obsClass.maxNegatedHeight).max
        )
      }
    }
  }

  def True[A]: And[A] = And[A](Set())

  case class Observe[A](action: A, andThen: Formula[A]) extends Formula[A] {
    override def toString = "⟨" + action.toString + "⟩" + andThen.toString

    override val isPositive = true
    
    override val obsClass = ObservationClass(andThen.obsClass.observationHeight + 1,
      andThen.obsClass.conjunctionLevels + (if (!andThen.isPositive) 1 else 0),0,0,0,0) lub andThen.obsClass

  }
  case class Negate[A](andThen: Formula[A]) extends Formula[A] {
    override def toString = "¬" + andThen.toString

    override val isPositive = false

    override val obsClass = ObservationClass(0, andThen.obsClass.conjunctionLevels + (if (!andThen.isPositive) 1 else 0),
      0,0,andThen.obsClass.negationLevels + 1,andThen.obsClass.observationHeight) lub andThen.obsClass

  }

}