package io.equiv.eqfiddle.hml

case class ObservationClassEnergyWeak(
  /** the maximal observation depth of the subformulas (⊤ has height 0, negation and conjunction are neutral wrt. height) */
  observationHeight: Int = 0,
  /** are there branching observations among the observationHeight levels */
  branchingObservations: Int = 0,
  /** the maximal amount of conjunctions when descending into a formula */
  conjunctionLevels: Int = 0,
  /** number of “strong” conjunctions among the conjunctionLevels */
  immediateConjunctions: Int = 0,
  /** the maximal height of revivals at local-test conjunctions (observationHeight)*/
  revivalHeight: Int = 0,
  /** maximal height of positive conjunctions (observationHeight) excluding revivals*/
  positiveConjHeight: Int = 0,
  /** maximal height of negative conjuncts */
  negativeConjHeight: Int = 0,
  /** how many negation levels may happen? */
  negationLevels: Int = 0
) extends ObservationClass {

  override def tryCompareTo[B >: ObservationClass](that: B)(implicit evidence$1: B => PartiallyOrdered[B]): Option[Int] = {
    that match {
      case that: ObservationClassEnergyWeak =>
        if (this == that) {
          Some(0)
        } else if (
            this.observationHeight >= that.observationHeight &&
            this.branchingObservations >= that.branchingObservations &&
            this.conjunctionLevels >= that.conjunctionLevels &&
            this.immediateConjunctions >= that.immediateConjunctions &&
            this.revivalHeight >= that.revivalHeight &&
            this.positiveConjHeight >= that.positiveConjHeight &&
            this.negativeConjHeight >= that.negativeConjHeight &&
            this.negationLevels >= that.negationLevels) {
          Some(1)
        } else if (
            this.observationHeight <= that.observationHeight &&
            this.branchingObservations <= that.branchingObservations &&
            this.conjunctionLevels <= that.conjunctionLevels &&
            this.immediateConjunctions <= that.immediateConjunctions &&
            this.revivalHeight <= that.revivalHeight &&
            this.positiveConjHeight <= that.positiveConjHeight &&
            this.negativeConjHeight <= that.negativeConjHeight &&
            this.negationLevels <= that.negationLevels) {
          Some(-1)
        } else {
          None
        }
      case _ => None
    }
  }

  override def lub[B >: ObservationClassEnergyWeak](that: B): B = that match {
    case that: ObservationClassEnergyWeak =>
      ObservationClassEnergyWeak(
        Integer.max(this.observationHeight, that.observationHeight),
        Integer.max(this.branchingObservations, that.branchingObservations),
        Integer.max(this.conjunctionLevels, that.conjunctionLevels),
        Integer.max(this.immediateConjunctions, that.immediateConjunctions),
        Integer.max(this.revivalHeight, that.revivalHeight),
        Integer.max(this.positiveConjHeight, that.positiveConjHeight),
        Integer.max(this.negativeConjHeight, that.negativeConjHeight),
        Integer.max(this.negationLevels, that.negationLevels)
      )
    case _ => this
  }

  override def glb[B >: ObservationClassEnergyWeak](that: B): B = that match {
    case that: ObservationClassEnergyWeak =>
      ObservationClassEnergyWeak(
        Integer.min(this.observationHeight, that.observationHeight),
        Integer.min(this.branchingObservations, that.branchingObservations),
        Integer.min(this.conjunctionLevels, that.conjunctionLevels),
        Integer.min(this.immediateConjunctions, that.immediateConjunctions),
        Integer.min(this.revivalHeight, that.revivalHeight),
        Integer.min(this.positiveConjHeight, that.positiveConjHeight),
        Integer.min(this.negativeConjHeight, that.negativeConjHeight),
        Integer.min(this.negationLevels, that.negationLevels)
      )
    case _ => this
  }

  override def toTuple = (observationHeight, branchingObservations, conjunctionLevels, immediateConjunctions, revivalHeight, positiveConjHeight, negativeConjHeight, negationLevels)
}

object ObservationClassEnergyWeak {
  val INFTY = Integer.MAX_VALUE

  // observationHeight, branchingObservations, conjunctionLevels, immediateConjunctions, revivalHeight, positiveConjHeight, negativeConjHeight, negationLevels
  // The Linear-time Branching-time Spectrum
  val BaseLTBTS = List(
    "enabledness" ->           ObservationClassEnergyWeak(    1,     0,     0,     0,     0,    0,    0,    0),
    "traces" ->                ObservationClassEnergyWeak(INFTY,     0,     0,     0,     0,    0,    0,    0),
    "failure" ->               ObservationClassEnergyWeak(INFTY,     0,     1,     0,     0,    0,    1,    1),
    "readiness" ->             ObservationClassEnergyWeak(INFTY,     0,     1,     0,     1,    1,    1,    1),
    //"failure-trace" ->         ObservationClassEnergyWeak(INFTY,     0, INFTY,     0, INFTY,    0,    1,    1),
    //"ready-trace" ->           ObservationClassEnergyWeak(INFTY,     0, INFTY,     0, INFTY,    1,    1,    1),
    "impossible-future" ->     ObservationClassEnergyWeak(INFTY,     0,     1,     0,     0,    0,INFTY,    1),
    "possible-future" ->       ObservationClassEnergyWeak(INFTY,     0,     1,     0, INFTY,INFTY,INFTY,    1),
    "simulation" ->            ObservationClassEnergyWeak(INFTY,     0, INFTY,     0, INFTY,INFTY,    0,    0),
    "ready-simulation" ->      ObservationClassEnergyWeak(INFTY,     0, INFTY,     0, INFTY,INFTY,    1,    1),
    "2-nested-simulation"->    ObservationClassEnergyWeak(INFTY,     0, INFTY,     0, INFTY,INFTY,INFTY,    1),
    "contrasimulation" ->      ObservationClassEnergyWeak(INFTY,     0, INFTY,     0,     0,    0,INFTY,INFTY),
    "weak-bisimulation" ->     ObservationClassEnergyWeak(INFTY,     0, INFTY,     0, INFTY,INFTY,INFTY,INFTY),
    "delay-bisimulation" ->    ObservationClassEnergyWeak(INFTY,     0, INFTY, INFTY, INFTY,INFTY,INFTY,INFTY),
    "eta-bisimulation"   ->    ObservationClassEnergyWeak(INFTY,     1, INFTY,     0, INFTY,INFTY,INFTY,INFTY),
    "branching-bisimulation"-> ObservationClassEnergyWeak(INFTY,     1, INFTY, INFTY, INFTY,INFTY,INFTY,INFTY)
  )


  val LTBTS = Spectrum.fromTuples(BaseLTBTS, getFormulaRootClass)

  def formulaObsClass(f: HennessyMilnerLogic.Formula[_]): ObservationClassEnergyWeak = f match {
    case HennessyMilnerLogic.And(subterms) =>
      if (subterms.isEmpty) {
        ObservationClassEnergyWeak()
      } else {
        val (positiveSubterms, negativeSubterms) = subterms.toList.partition(_.isPositive)
        val (positiveFlat, positiveDeep) = positiveSubterms.map(formulaObsClass(_)).partition(_.observationHeight <= 1)
        val (negativeFlat, negativeDeep) = negativeSubterms.map(formulaObsClass(_)).partition(_.observationHeight <= 1)
        val allClasses = positiveDeep ++ positiveFlat ++ negativeDeep ++ negativeFlat
        val isBranchingObs = positiveSubterms.collect { case HennessyMilnerLogic.Observe(a, _) => a}.size

        // if (allClasses.isEmpty || negativeDeep.nonEmpty || positiveDeep.size > 1) {
          ObservationClassEnergyWeak(
            observationHeight = allClasses.map(_.observationHeight).max,
            branchingObservations = (isBranchingObs::allClasses.map(_.branchingObservations)).max,
            conjunctionLevels = allClasses.map(_.conjunctionLevels).max + 1,
            immediateConjunctions = allClasses.map(_.immediateConjunctions).max + 1,
            revivalHeight = allClasses.map(_.revivalHeight).max,
            positiveConjHeight = (allClasses.map(_.positiveConjHeight) ++ (positiveFlat ++ positiveDeep).map(_.observationHeight)).max,
            negativeConjHeight = (allClasses.map(_.negativeConjHeight) ++ (negativeFlat ++ negativeDeep).map(_.observationHeight)).max,
            negationLevels = allClasses.map(_.negationLevels).max,
          )
        // } else {
        //   // this conjunction can be understood as a local observation
        //   val revivalDepth = (positiveDeep.headOption orElse positiveFlat.headOption).map(_.observationHeight).getOrElse(0)
        //   ObservationClassEnergyWeak(
        //     observationHeight = allClasses.map(_.observationHeight).max,
        //     branchingObservations = allClasses.map(_.branchingObservations).max,
        //     conjunctionLevels = allClasses.map(_.conjunctionLevels).max + 1,
        //     immediateConjunctions = allClasses.map(_.immediateConjunctions).max + 1,
        //     revivalHeight = Integer.max(revivalDepth, allClasses.map(_.revivalHeight).max),
        //     positiveConjHeight =
        //       if (positiveFlat.nonEmpty) Integer.max(1, allClasses.map(_.positiveConjHeight).max) else allClasses.map(_.positiveConjHeight).max,
        //     negativeConjHeight =
        //       if (negativeFlat.nonEmpty) Integer.max(1, allClasses.map(_.negativeConjHeight).max) else allClasses.map(_.negativeConjHeight).max,
        //     negationLevels = allClasses.map(_.negationLevels).max,
        //   )
        //}
      }
    case HennessyMilnerLogic.Negate(andThen) =>
      val andThenClass = formulaObsClass(andThen)
      ObservationClassEnergyWeak(
        negationLevels = andThenClass.negationLevels + 1
      ) lub andThenClass
    case HennessyMilnerLogic.Observe(action, andThen) =>
      val andThenClass = formulaObsClass(andThen)
      ObservationClassEnergyWeak(
        observationHeight = andThenClass.observationHeight + 1
      ) lub andThenClass
    case HennessyMilnerLogic.Pass(andThen) =>
      val andThenClass = formulaObsClass(andThen)
      if (andThen.isInstanceOf[HennessyMilnerLogic.And[_]]) {
        ObservationClassEnergyWeak(
          observationHeight = andThenClass.observationHeight,
          branchingObservations = andThenClass.branchingObservations,
          conjunctionLevels = andThenClass.conjunctionLevels,
          immediateConjunctions = andThenClass.immediateConjunctions - 1, // decreasing!
          revivalHeight = andThenClass.revivalHeight,
          positiveConjHeight = andThenClass.positiveConjHeight,
          negativeConjHeight = andThenClass.negativeConjHeight,
          negationLevels = andThenClass.negationLevels,
        )
      } else {
        andThenClass
      }
  }

  def getFormulaRootClass(f: HennessyMilnerLogic.Formula[_]) = {
    if (f.isPositive) {
      formulaObsClass(f)
    } else {
      formulaObsClass(HennessyMilnerLogic.And(Set(f)))
    }
  }

}