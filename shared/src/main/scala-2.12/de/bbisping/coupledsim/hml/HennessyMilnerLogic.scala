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
        obsClass.nonNegativeConjuncts,
        obsClass.immediatePostObs,
        obsClass.immediateConj || !isPositive,
        obsClass.etaConjObs
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
        ObservationClass(0,0,0,0,0,0,false,false,false,0)
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
          nonNegativeConjuncts = subterms.exists(f => f.isPositive || f.obsClass.nonNegativeConjuncts),
          /** if there are observations that are not immediately followed by possible internal activity */
          immediatePostObs = subterms.exists(_.obsClass.immediatePostObs),
          /** if there are conjunctions / negations that are not immediately preceeded by possible internal activity */
          immediateConj = true,
          /** how many immediate observations may occur within weak conjunctions? */
          etaConjObs = subterms.map(_.obsClass.etaConjObs).max
        )
      }
    }
  }

  case class Observe[A](action: A, andThen: Formula[A]) extends Formula[A] {
    override def toString = "⟨" + action.toString + "⟩" + andThen.toString

    override val isPositive = true
    
    override val obsClass =
      ObservationClass(
        height = andThen.obsClass.height + 1,
        conjunctionLevels = andThen.obsClass.conjunctionLevels + (if (!andThen.isPositive) 1 else 0),
        immediatePostObs = !andThen.isInstanceOf[Pass[A]],
        immediateConj = andThen.isInstanceOf[And[A]] || !andThen.isPositive
      ) lub andThen.obsClass
  }

  case class Pass[A](andThen: Formula[A]) extends Formula[A] {
    override def toString = "⟨ϵ⟩" + andThen.toString

    override val isPositive = true
    
    override val obsClass = ObservationClass(
      height = andThen.obsClass.height,
      conjunctionLevels = andThen.obsClass.conjunctionLevels,
      negationLevels = andThen.obsClass.negationLevels,
      maxPositiveDeepBranches = andThen.obsClass.maxPositiveDeepBranches,
      maxPositiveFlatBranches = andThen.obsClass.maxPositiveFlatBranches,
      maxNegationHeight = andThen.obsClass.maxNegationHeight,
      nonNegativeConjuncts = andThen.obsClass.nonNegativeConjuncts,
      immediateConj = andThen match {
        case And(subterms) =>
          subterms.exists(_.obsClass.immediateConj)
        case _ =>
          andThen.obsClass.immediateConj
      },
      etaConjObs = andThen match {
        case And(subterms) =>
          Integer.max(
            subterms.map(_.obsClass.etaConjObs).max,
            subterms.count(_.isInstanceOf[Observe[A]]))
        case _ =>
          0
      },
      immediatePostObs = andThen.obsClass.immediatePostObs
    )

  }
  case class Negate[A](andThen: Formula[A]) extends Formula[A] {
    override def toString = "¬" + andThen.toString

    override val isPositive = false

    override val obsClass = ObservationClass(
      conjunctionLevels = andThen.obsClass.conjunctionLevels + (if (!andThen.isPositive) 1 else 0),
      negationLevels = andThen.obsClass.negationLevels + 1,
      maxNegationHeight = andThen.obsClass.height
    ) lub andThen.obsClass

  }

}