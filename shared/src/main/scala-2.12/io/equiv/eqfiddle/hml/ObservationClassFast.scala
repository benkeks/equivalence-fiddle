package io.equiv.eqfiddle.hml

case class ObservationClassFast(
  /** the maximal observation depth of the subformulas (⊤ has height 0, negation and conjunction are neutral wrt. height) */
  observationHeight: Int = 0,
  /** the maximal amount of conjunctions when descending into a formula */
  conjunctionLevels: Int = 0,
  /** the maximal height of failure-test conjunctions (observationHeight)*/
  maxFailureHeight: Int = 0,
  /** the maximal height of readiness-test conjunctions (observationHeight)*/
  maxReadinessHeight: Int = 0,
  /** how many deep negative conjunct levels may happen? */
  deepNegativeConjunctionLevels: Int = 0,
  /** how many deep positive conjunct levels may happen? */
  deepPositiveConjunctionLevels: Int = 0
) extends ObservationClass {

  override def tryCompareTo[B >: ObservationClass](that: B)(implicit evidence$1: B => PartiallyOrdered[B]): Option[Int] = {
    that match {
      case that: ObservationClassFast =>
        if (this == that) {
          Some(0)
        } else if (
            this.observationHeight >= that.observationHeight &&
            this.conjunctionLevels >= that.conjunctionLevels &&
            this.maxFailureHeight >= that.maxFailureHeight &&
            this.maxReadinessHeight >= that.maxReadinessHeight &&
            this.deepNegativeConjunctionLevels >= that.deepNegativeConjunctionLevels &&
            this.deepPositiveConjunctionLevels >= that.deepPositiveConjunctionLevels) {
          Some(1)
        } else if (
            this.observationHeight <= that.observationHeight &&
            this.conjunctionLevels <= that.conjunctionLevels &&
            this.maxFailureHeight <= that.maxFailureHeight &&
            this.maxReadinessHeight <= that.maxReadinessHeight &&
            this.deepNegativeConjunctionLevels <= that.deepNegativeConjunctionLevels &&
            this.deepPositiveConjunctionLevels <= that.deepPositiveConjunctionLevels) {
          Some(-1)
        } else {
          None
        }
      case _ => None
    }
  }

  override def lub[B >: ObservationClassFast](that: B): B = that match {
    case that: ObservationClassFast =>
      ObservationClassFast(
        Integer.max(this.observationHeight, that.observationHeight),
        Integer.max(this.conjunctionLevels, that.conjunctionLevels),
        Integer.max(this.maxFailureHeight, that.maxFailureHeight),
        Integer.max(this.maxReadinessHeight, that.maxReadinessHeight),
        Integer.max(this.deepNegativeConjunctionLevels, that.deepNegativeConjunctionLevels),
        Integer.max(this.deepPositiveConjunctionLevels, that.deepPositiveConjunctionLevels)
      )
    case _ => this
  }

  override def glb[B >: ObservationClassFast](that: B): B = that match {
    case that: ObservationClassFast =>
      ObservationClassFast(
        Integer.min(this.observationHeight, that.observationHeight),
        Integer.min(this.conjunctionLevels, that.conjunctionLevels),
        Integer.min(this.maxFailureHeight, that.maxFailureHeight),
        Integer.min(this.maxReadinessHeight, that.maxReadinessHeight),
        Integer.min(this.deepNegativeConjunctionLevels, that.deepNegativeConjunctionLevels),
        Integer.min(this.deepPositiveConjunctionLevels, that.deepPositiveConjunctionLevels)
      )
    case _ => this
  }

  override def toTuple = (observationHeight, conjunctionLevels, maxFailureHeight, maxReadinessHeight, deepNegativeConjunctionLevels, deepPositiveConjunctionLevels)
}

object ObservationClassFast {
  val INFTY = Integer.MAX_VALUE

  // observationHeight, conjunctionLevels, maxFailureHeight, maxReadinessHeight, deepNegativeConjunctionLevels, deepPositiveConjunctionLevels
  // The Linear-time Branching-time Spectrum
  val BaseLTBTS = List(
    "enabledness" ->        ObservationClassFast(    1,     0,    0,    0,    0,    0),
    "traces" ->             ObservationClassFast(INFTY,     0,    0,    0,    0,    0),
    "failure" ->            ObservationClassFast(INFTY,     1,    1,    0,    0,    0),
    "readiness" ->          ObservationClassFast(INFTY,     1,    1,    1,    0,    0),
    "failure-trace" ->      ObservationClassFast(INFTY, INFTY,INFTY,    0,    0,    0),
    "ready-trace" ->        ObservationClassFast(INFTY, INFTY,INFTY,INFTY,    0,    0),
    "impossible-future" ->  ObservationClassFast(INFTY,     1,    1,    0,    1,    0),
    "possible-future" ->    ObservationClassFast(INFTY,     1,INFTY,INFTY,    1,    1),
    "simulation" ->         ObservationClassFast(INFTY, INFTY,    0,INFTY,    0,INFTY),
    "ready-simulation" ->   ObservationClassFast(INFTY, INFTY,INFTY,INFTY,    0,INFTY),
    "2-nested-simulation"-> ObservationClassFast(INFTY, INFTY,INFTY,INFTY,    1,INFTY), // TODO: Double spending problem of negations!
    "bisimulation" ->       ObservationClassFast(INFTY, INFTY,INFTY,INFTY,INFTY,INFTY)
  )


  val LTBTS = Spectrum.fromTuples(BaseLTBTS, getFormulaRootClass)

  def formulaObsClass(f: HennessyMilnerLogic.Formula[_]): ObservationClassFast = f match {
    case HennessyMilnerLogic.And(subterms) =>
      if (subterms.isEmpty) {
        ObservationClassFast()
      } else {
        val (positiveSubterms, negativeSubterms) = subterms.toList.partition(_.isPositive)
        val (positiveFlat, positiveDeep) = positiveSubterms.map(formulaObsClass(_)).partition(_.observationHeight <= 1)
        val (negativeFlat, negativeDeep) = negativeSubterms.map(formulaObsClass(_)).partition(_.observationHeight <= 1)
        val allClasses = positiveDeep ++ positiveFlat ++ negativeDeep ++ negativeFlat

        if (allClasses.isEmpty || negativeDeep.nonEmpty || positiveDeep.size > 1) {
          ObservationClassFast(
            observationHeight = allClasses.map(_.observationHeight).max,
            conjunctionLevels = allClasses.map(_.conjunctionLevels).max + 1,
            maxFailureHeight = allClasses.map(_.maxFailureHeight).max,
            maxReadinessHeight = allClasses.map(_.maxReadinessHeight).max,
            deepNegativeConjunctionLevels =
              allClasses.map(_.deepNegativeConjunctionLevels).max + (if (negativeFlat.nonEmpty || negativeDeep.nonEmpty) 1 else 0),
            deepPositiveConjunctionLevels =
              allClasses.map(_.deepPositiveConjunctionLevels).max + (if (positiveFlat.nonEmpty || positiveDeep.nonEmpty) 1 else 0),
          )
        } else {
          // this conjunction can be understood as a local observation
          val revivalDepth = (positiveDeep.headOption).map(_.observationHeight).getOrElse(1)
          ObservationClassFast(
            observationHeight = allClasses.map(_.observationHeight).max,
            conjunctionLevels = allClasses.map(_.conjunctionLevels).max + 1,
            maxFailureHeight =
              if (negativeFlat.nonEmpty) Integer.max(revivalDepth, allClasses.map(_.maxFailureHeight).max) else allClasses.map(_.maxFailureHeight).max,
            maxReadinessHeight =
              if (positiveFlat.nonEmpty) Integer.max(revivalDepth, allClasses.map(_.maxReadinessHeight).max) else allClasses.map(_.maxReadinessHeight).max,
            deepNegativeConjunctionLevels = allClasses.map(_.deepNegativeConjunctionLevels).max,
            deepPositiveConjunctionLevels = allClasses.map(_.deepPositiveConjunctionLevels).max,
          )
        }
      }
    case HennessyMilnerLogic.Negate(andThen) =>
      formulaObsClass(andThen)
    case HennessyMilnerLogic.Observe(action, andThen) =>
      val andThenClass = formulaObsClass(andThen)
      ObservationClassFast(
        observationHeight = andThenClass.observationHeight + 1
      ) lub andThenClass
    case HennessyMilnerLogic.Pass(andThen) =>
      formulaObsClass(andThen)
  }

  def getFormulaRootClass(f: HennessyMilnerLogic.Formula[_]) = {
    if (f.isPositive) {
      formulaObsClass(f)
    } else {
      formulaObsClass(HennessyMilnerLogic.And(Set(f)))
    }
  }

}
