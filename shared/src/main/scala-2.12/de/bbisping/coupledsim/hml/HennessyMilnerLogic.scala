package de.bbisping.coupledsim.hml

object HennessyMilnerLogic {


  def getLeastDistinguishing[A](formulas: Set[Formula[A]]): Set[Formula[A]] = {
    val classifications = formulas.map(f => (f, f.classifyFormula()))
    val bounds = classifications.flatMap(_._2._2.map(_._2))

    for {
      (f, (cl, clB)) <- classifications
      clBb = clB.map(_._2)
      // just keep formulas where one of the classifications is dominated by no other classification
      if clBb.exists(classBound => !bounds.exists(_ strictlyBelow classBound))
    } yield f
  }

  abstract sealed class Formula[A] {

    def obsClass: ObservationClass

    def isPositive: Boolean

    /** class of this formula if it appears at the top level */
    def getRootClass() = {
      ObservationClass(
        obsClass.height,
        obsClass.conjunctionLevels + (if (!isPositive) 1 else 0),
        obsClass.negationLevels,
        obsClass.maxPositiveDeepBranches,
        obsClass.maxPositiveFlatBranches,
        obsClass.maxNegationHeight,
        obsClass.nonNegativeConjuncts
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
        ObservationClass(0,0,0,0,0,0,false)
      } else {
        val (deepSubtermsPrelim, flatSubtermsPrelim) = subterms.partition(_.obsClass.height > 1)
        val positiveFlat = flatSubtermsPrelim.find(_.isPositive)
        
        val (deepSubterms, flatSubterms) = if (deepSubtermsPrelim.isEmpty && positiveFlat.nonEmpty)
          (positiveFlat.toList, flatSubtermsPrelim - positiveFlat.get)
        else
          (deepSubtermsPrelim, flatSubtermsPrelim)

        ObservationClass(
          height = subterms.map(_.obsClass.height).max + 1,
          /** the maximal amount of conjunctions when descending into a formula */
          conjunctionLevels = subterms.map(_.obsClass.conjunctionLevels).max + 1,
          /** the maximal amount of negations when descending into a formula */
          negationLevels = subterms.map(_.obsClass.negationLevels).max,
          /** the maximal amount of positive deep branches */
          maxPositiveDeepBranches = (subterms.map(_.obsClass.maxPositiveDeepBranches) + deepSubtermsPrelim.count(_.isPositive)).max,
          /** the maximal amount of positive flat branches (height > 1); if all branches are flat, one positive branch will be exempted from the count */
          maxPositiveFlatBranches = (subterms.map(_.obsClass.maxPositiveFlatBranches) + flatSubterms.count(f => f.isPositive)).max,
          /** maximal height of negative subformulas */
          maxNegationHeight = subterms.map(_.obsClass.maxNegationHeight).max,
          nonNegativeConjuncts = subterms.exists(f => f.isPositive || f.obsClass.nonNegativeConjuncts)
        )
      }
    }
  }

  case class Observe[A](action: A, andThen: Formula[A]) extends Formula[A] {
    override def toString = "⟨" + action.toString + "⟩" + andThen.toString

    override val isPositive = true
    
    override val obsClass = ObservationClass(andThen.obsClass.height + 1, andThen.obsClass.conjunctionLevels + (if (!andThen.isPositive) 1 else 0),0,0,0,0, false) lub andThen.obsClass

  }

  case class Negate[A](andThen: Formula[A]) extends Formula[A] {
    override def toString = "¬" + andThen.toString

    override val isPositive = false

    override val obsClass = ObservationClass(0, andThen.obsClass.conjunctionLevels + (if (!andThen.isPositive) 1 else 0),andThen.obsClass.negationLevels + 1,0,0,andThen.obsClass.height,false) lub andThen.obsClass

  }

}