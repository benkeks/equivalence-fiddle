package de.bbisping.coupledsim.hml

object HennessyMilnerLogic {

  case class ObservationClass(
    /** the maximal amount of conjunctions when descending into a formula */
    conjunctionLevels: Int,
    /** the maximal amount of negations when descending into a formula */
    negationLevels: Int,
    /** the maximal amount of positive deep branches */
    maxPositiveDeepBranches: Int,
    /** the maximal amount of negative deep branches */
    maxNegativeDeepBranches: Int,
    /** the maximal amount of positive flat branches (height > 1); if all branches are flat, one positive branch will be counted as deep */
    maxPositiveFlatBranches: Int,
    /** maximal height of negative subformulas */
    maxNegationHeight: Int,
    /** how many negation levels are not immediately following upon a conjunction? */
    nonImmediateNegations: Int,
    /** if there are any conjunctions with positive subformulas */
    nonNegativeConjuncts: Boolean
  ) {
    def lub(that: ObservationClass) = ObservationClass(
      Integer.max(this.conjunctionLevels, that.conjunctionLevels),
      Integer.max(this.negationLevels, that.negationLevels),
      Integer.max(this.maxPositiveDeepBranches, that.maxPositiveDeepBranches),
      Integer.max(this.maxNegativeDeepBranches, that.maxNegativeDeepBranches),
      Integer.max(this.maxPositiveFlatBranches, that.maxPositiveFlatBranches),
      Integer.max(this.maxNegationHeight, that.maxNegationHeight),
      Integer.max(this.nonImmediateNegations, that.nonImmediateNegations),
      this.nonNegativeConjuncts || that.nonNegativeConjuncts
    )

    def glb(that: ObservationClass) = ObservationClass(
      Integer.min(this.conjunctionLevels, that.conjunctionLevels),
      Integer.min(this.negationLevels, that.negationLevels),
      Integer.min(this.maxPositiveDeepBranches, that.maxPositiveDeepBranches),
      Integer.min(this.maxNegativeDeepBranches, that.maxNegativeDeepBranches),
      Integer.min(this.maxPositiveFlatBranches, that.maxPositiveFlatBranches),
      Integer.min(this.maxNegationHeight, that.maxNegationHeight),
      Integer.min(this.nonImmediateNegations, that.nonImmediateNegations),
      this.nonNegativeConjuncts && that.nonNegativeConjuncts
    )


    def above(that: ObservationClass) = (
      this.conjunctionLevels >= that.conjunctionLevels &&
      this.negationLevels >= that.negationLevels &&
      this.maxPositiveDeepBranches >= that.maxPositiveDeepBranches &&
      this.maxNegativeDeepBranches >= that.maxNegativeDeepBranches &&
      this.maxPositiveFlatBranches >= that.maxPositiveFlatBranches &&
      this.maxNegationHeight >= that.maxNegationHeight &&
      this.nonImmediateNegations >= that.nonImmediateNegations &&
      (this.nonNegativeConjuncts || !that.nonNegativeConjuncts)
    )

    def below(that: ObservationClass) = (
      this.conjunctionLevels <= that.conjunctionLevels &&
      this.negationLevels <= that.negationLevels &&
      this.maxPositiveDeepBranches <= that.maxPositiveDeepBranches &&
      this.maxNegativeDeepBranches <= that.maxNegativeDeepBranches &&
      this.maxPositiveFlatBranches <= that.maxPositiveFlatBranches &&
      this.maxNegationHeight <= that.maxNegationHeight &&
      this.nonImmediateNegations <= that.nonImmediateNegations &&
      (!this.nonNegativeConjuncts || that.nonNegativeConjuncts)
    )

    def balance = ObservationClass(
      this.conjunctionLevels,
      this.negationLevels,
      this.maxPositiveDeepBranches,
      this.maxNegativeDeepBranches,
      this.maxPositiveFlatBranches,
      this.maxNegationHeight,
      this.nonImmediateNegations - (if (conjunctionLevels == 0) 1 else 0),
      this.nonNegativeConjuncts
    )
  }
  val INFTY = 9999999

  val ObservationClasses = List(
    "traces" -> ObservationClass(0,0,0,0,0,0,0,false),
    "failure" -> ObservationClass(1,1,0,0,0,1,0,false),
    "readiness" -> ObservationClass(1,1,0,0,INFTY,1,0,true),
    "failure-trace" -> ObservationClass(INFTY,1,1,0,0,1,0,true),
    "ready-trace" -> ObservationClass(INFTY,1,1,0,INFTY,1,0,true),
    "impossible-future" -> ObservationClass(1,1,0,INFTY,0,INFTY,0,false),
    "possible-future" -> ObservationClass(1,1,INFTY,INFTY,0,INFTY,0,true),
    "simulation" -> ObservationClass(INFTY,0,INFTY,INFTY,INFTY,INFTY,INFTY,true),
    "ready-simulation" -> ObservationClass(INFTY,1,INFTY,INFTY,INFTY,INFTY,0,true),
    "2-nested-simulation" -> ObservationClass(INFTY,1,INFTY,INFTY,INFTY,INFTY,INFTY,true),
    "bisimulation" -> ObservationClass(INFTY,INFTY,INFTY,INFTY,INFTY,INFTY,INFTY,true)
  )

  abstract sealed class Formula[A] {

    /** the maximal depth of the subformulas (⊤ has height 0, negation are neutral wrt. height) */
    def height: Int

    def obsClass: ObservationClass

    def isPositive: Boolean

    /** names the coarsest notion of equivalence where this formula is part of the distinguishing formulas */
    def classifyFormula(): String = {
      val balancedClass = obsClass.balance
      val classifications = ObservationClasses.collect { case (name, cl) if (balancedClass lub cl) == cl => (name, cl) }
      var currentMax = List[ObservationClass]()
      val leastClassifications = for {
        (name, cl) <- classifications
        if !currentMax.exists(cl.above(_))
      } yield {
        currentMax = cl :: currentMax
        name
      }
      balancedClass + (leastClassifications mkString ",")
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

    override val height: Int = if (subterms.nonEmpty) {
      subterms.map(_.height).max + 1
    } else {
      0
    }

    override val isPositive = true

    override val obsClass = {
      
      if (subterms.isEmpty) {
        ObservationClass(0,0,0,0,0,0,0,false)
      } else {
        val (deepSubtermsPrelim, flatSubtermsPrelim) = subterms.partition(_.height > 1)
        val positiveFlat = flatSubtermsPrelim.find(_.isPositive)
        
        val (deepSubterms, flatSubterms) = if (deepSubtermsPrelim.isEmpty && positiveFlat.nonEmpty)
          (positiveFlat.toList, flatSubtermsPrelim - positiveFlat.get)
        else
          (deepSubtermsPrelim, flatSubtermsPrelim)

        ObservationClass(
          /** the maximal amount of conjunctions when descending into a formula */
          conjunctionLevels = subterms.map(_.obsClass.conjunctionLevels).max + 1,
          /** the maximal amount of negations when descending into a formula */
          negationLevels = subterms.map(_.obsClass.negationLevels).max,
          /** the maximal amount of positive deep branches */
          maxPositiveDeepBranches = (subterms.map(_.obsClass.maxPositiveDeepBranches) + deepSubtermsPrelim.count(_.isPositive)).max,
          /** the maximal amount of negative deep branches */
          maxNegativeDeepBranches  = (subterms.map(_.obsClass.maxNegativeDeepBranches) + deepSubtermsPrelim.count(f => !f.isPositive)).max,
          /** the maximal amount of positive flat branches (height > 1); if all branches are flat, one positive branch will be exempted from the count */
          maxPositiveFlatBranches = (subterms.map(_.obsClass.maxPositiveFlatBranches) + flatSubterms.count(f => f.isPositive)).max,
          /** maximal height of negative subformulas */
          maxNegationHeight = subterms.map(_.obsClass.maxNegationHeight).max,
          /** if there are conjunctions, are there any negations not immediately following upon a conjunction */
          nonImmediateNegations = (subterms.map {
            case Negate(andThen) => andThen.obsClass.nonImmediateNegations
            case o => o.obsClass.nonImmediateNegations
          }).max,
          nonNegativeConjuncts = subterms.exists(f => f.isPositive || f.obsClass.nonNegativeConjuncts)
        )
      }
    }
  }

  case class Observe[A](action: A, andThen: Formula[A]) extends Formula[A] {
    override def toString = "⟨" + action.toString + "⟩" + andThen.toString

    override val height: Int = andThen.height + 1

    override val isPositive = true
    
    override val obsClass = ObservationClass(0,0,0,0,0,0, andThen.obsClass.nonImmediateNegations + (if (!andThen.isPositive) 1 else 0), false) lub andThen.obsClass

  }

  case class Negate[A](andThen: Formula[A]) extends Formula[A] {
    override def toString = "¬" + andThen.toString

    override val height: Int = andThen.height

    override val isPositive = false

    override val obsClass = ObservationClass(0,andThen.obsClass.negationLevels + 1,0,0,0,height,andThen.obsClass.nonImmediateNegations + (if (!andThen.isPositive) 1 else 0),false) lub andThen.obsClass

  }

}