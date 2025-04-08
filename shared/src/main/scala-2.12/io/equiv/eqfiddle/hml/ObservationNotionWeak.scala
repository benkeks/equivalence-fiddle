package io.equiv.eqfiddle.hml

case class WeakObservationNotion(
  /** the maximal observation depth of the subformulas (⊤ has height 0, negation and conjunction are neutral wrt. height) */
  observationHeight: Int = 0,
  /** the maximal amount of branching conjunctions when descending into a formula */
  branchingConjunctionLevels: Int = 0,
  /** the maximal amount of unstable conjunctions when descending into a formula */
  unstableConjunctionLevels: Int = 0,
  /** the maximal amount of stable conjunctions when descending into a formula */
  stableConjunctionLevels: Int = 0,
  /** number of “strong” conjunctions among the conjunctionLevels */
  immediateConjunctionLevels: Int = 0,
  /** maximal height of positive conjuncts (observationHeight) */
  positiveConjHeight: Int = 0,
  /** maximal height of positive conjuncts (observationHeight) at stable conjunctions excluding revival conjunct */
  positiveConjSecondaryHeight: Int = 0,
  /** maximal height of negative conjuncts */
  negativeConjHeight: Int = 0,
  /** how many negation levels may happen? */
  negationLevels: Int = 0
) extends ObservationNotion {

  override def tryCompareTo[B >: ObservationNotion](that: B)(implicit evidence$1: B => PartiallyOrdered[B]): Option[Int] = {
    that match {
      case that: WeakObservationNotion =>
        if (this == that) {
          Some(0)
        } else if (
            this.observationHeight >= that.observationHeight &&
            this.branchingConjunctionLevels >= that.branchingConjunctionLevels &&
            this.unstableConjunctionLevels >= that.unstableConjunctionLevels &&
            this.stableConjunctionLevels >= that.stableConjunctionLevels &&
            this.immediateConjunctionLevels >= that.immediateConjunctionLevels &&
            this.positiveConjHeight >= that.positiveConjHeight &&
            this.positiveConjSecondaryHeight >= that.positiveConjSecondaryHeight &&
            this.negativeConjHeight >= that.negativeConjHeight &&
            this.negationLevels >= that.negationLevels) {
          Some(1)
        } else if (
            this.observationHeight <= that.observationHeight &&
            this.branchingConjunctionLevels <= that.branchingConjunctionLevels &&
            this.unstableConjunctionLevels <= that.unstableConjunctionLevels &&
            this.stableConjunctionLevels <= that.stableConjunctionLevels &&
            this.immediateConjunctionLevels <= that.immediateConjunctionLevels &&
            this.positiveConjHeight <= that.positiveConjHeight &&
            this.positiveConjSecondaryHeight <= that.positiveConjSecondaryHeight &&
            this.negativeConjHeight <= that.negativeConjHeight &&
            this.negationLevels <= that.negationLevels) {
          Some(-1)
        } else {
          None
        }
      case _ => None
    }
  }

  override def lub[B >: WeakObservationNotion](that: B): B = that match {
    case that: WeakObservationNotion =>
      WeakObservationNotion(
        Integer.max(this.observationHeight, that.observationHeight),
        Integer.max(this.branchingConjunctionLevels, that.branchingConjunctionLevels),
        Integer.max(this.unstableConjunctionLevels, that.unstableConjunctionLevels),
        Integer.max(this.stableConjunctionLevels, that.stableConjunctionLevels),
        Integer.max(this.immediateConjunctionLevels, that.immediateConjunctionLevels),
        Integer.max(this.positiveConjHeight, that.positiveConjHeight),
        Integer.max(this.positiveConjSecondaryHeight, that.positiveConjSecondaryHeight),
        Integer.max(this.negativeConjHeight, that.negativeConjHeight),
        Integer.max(this.negationLevels, that.negationLevels)
      )
    case _ => this
  }

  override def glb[B >: WeakObservationNotion](that: B): B = that match {
    case that: WeakObservationNotion =>
      WeakObservationNotion(
        Integer.min(this.observationHeight, that.observationHeight),
        Integer.min(this.branchingConjunctionLevels, that.branchingConjunctionLevels),
        Integer.min(this.unstableConjunctionLevels, that.unstableConjunctionLevels),
        Integer.min(this.stableConjunctionLevels, that.stableConjunctionLevels),
        Integer.min(this.immediateConjunctionLevels, that.immediateConjunctionLevels),
        Integer.min(this.positiveConjHeight, that.positiveConjHeight),
        Integer.min(this.positiveConjSecondaryHeight, that.positiveConjSecondaryHeight),
        Integer.min(this.negativeConjHeight, that.negativeConjHeight),
        Integer.min(this.negationLevels, that.negationLevels)
      )
    case _ => this
  }

  override def toTuple = (
    observationHeight,
    branchingConjunctionLevels,
    unstableConjunctionLevels,
    stableConjunctionLevels,
    immediateConjunctionLevels,
    positiveConjHeight,
    positiveConjSecondaryHeight,
    negativeConjHeight,
    negationLevels
  )
}

object WeakObservationNotion {
  val INFTY = Integer.MAX_VALUE

  // observationHeight, branchingConjLevels, unstableConjs, stableConjs, immediateConjs, positiveConjHeight, positiveConjSecondaryHeight, negativeConjHeight, negationLevels
  // The Linear-time Branching-time Spectrum
  val BaseLTBTS = List( 
    "weak-enabledness" ->             WeakObservationNotion(    1,     0,     0,     0,     0,     0,    0,    0,    0),
    "weak-trace" ->                   WeakObservationNotion(INFTY,     0,     0,     0,     0,     0,    0,    0,    0),
    "weak-failure" ->                 WeakObservationNotion(INFTY,     0,     1,     0,     0,     0,    0,    1,    1),
    "stable-failure" ->               WeakObservationNotion(INFTY,     0,     0,     1,     0,     0,    0,    1,    1),
    "weak-readiness" ->               WeakObservationNotion(INFTY,     0,     1,     0,     0,     1,    0,    1,    1),
    "stable-revivals" ->              WeakObservationNotion(INFTY,     0,     0,     1,     0,     1,    0,    1,    1),
    "stable-readiness" ->             WeakObservationNotion(INFTY,     0,     0,     1,     0,     1,    1,    1,    1),
    "stable-failure-trace" ->         WeakObservationNotion(INFTY,     0,     0, INFTY,     0, INFTY,    0,    1,    1),
    "stable-ready-trace" ->           WeakObservationNotion(INFTY,     0,     0, INFTY,     0, INFTY,    1,    1,    1),
    "s-impossible-future" ->          WeakObservationNotion(INFTY,     0,     0,     1,     0,     0,    0,INFTY,    1),
    "weak-impossible-future" ->       WeakObservationNotion(INFTY,     0,     1,     0,     0,     0,    0,INFTY,    1),
    "weak-possible-future" ->         WeakObservationNotion(INFTY,     0,     1,     0,     0, INFTY,    0,INFTY,    1),
  //"s-possible-future" ->       WeakObservationNotion(INFTY,     0,     0,     1,     0, INFTY,INFTY,INFTY,    1),
    "weak-simulation" ->              WeakObservationNotion(INFTY,     0, INFTY,     0,     0, INFTY,    0,    0,    0),
    "eta-simulation" ->               WeakObservationNotion(INFTY, INFTY, INFTY,     0,     0, INFTY,    0,    0,    0),
    "weak-ready-simulation" ->        WeakObservationNotion(INFTY,     0, INFTY,     0,     0, INFTY,    0,    1,    1),
    "stable-simulation" ->            WeakObservationNotion(INFTY,     0,     0, INFTY,     0, INFTY,INFTY,    0,    1),
    "s-ready-simulation" ->           WeakObservationNotion(INFTY,     0,     0, INFTY,     0, INFTY,INFTY,    1,    1),
    "2-nested-weak-simulation"->      WeakObservationNotion(INFTY,     0, INFTY,     0,     0, INFTY,    0,INFTY,    1),
    "contrasimulation" ->             WeakObservationNotion(INFTY,     0, INFTY,     0,     0,     0,    0,INFTY,INFTY),
    "stable-bisimulation" ->          WeakObservationNotion(INFTY,     0,     0, INFTY,     0, INFTY,INFTY,INFTY,INFTY),
    // sr-contrasimulation can only be added if we distinguish stable and instable positive conjuncts; otherwise we cannot position it such that it correctly dominates both contrasim and stable bisim without becoming too distinctive.
    //"sr-contrasimulation" ->          WeakObservationNotion(INFTY,     0, INFTY, INFTY,     0,     0,    0,INFTY,INFTY),
    "weak-bisimulation" ->            WeakObservationNotion(INFTY,     0, INFTY,     0,     0, INFTY,    0,INFTY,INFTY),
    //"sr-weak-bisimulation" ->    WeakObservationNotion(INFTY,     0, INFTY, INFTY,     0, INFTY,INFTY,INFTY,INFTY),
    "delay-bisimulation" ->           WeakObservationNotion(INFTY,     0, INFTY,     0, INFTY, INFTY,    0,INFTY,INFTY),
    "sr-delay-bisimulation" ->        WeakObservationNotion(INFTY,     0, INFTY, INFTY, INFTY, INFTY,INFTY,INFTY,INFTY),
    "eta-bisimulation"   ->           WeakObservationNotion(INFTY, INFTY, INFTY,     0,     0, INFTY,    0,INFTY,INFTY),
    //"sr-eta-bisimulation" ->     WeakObservationNotion(INFTY, INFTY, INFTY, INFTY,     0, INFTY,INFTY,INFTY,INFTY), //?
    "branching-bisimulation"->        WeakObservationNotion(INFTY, INFTY, INFTY,     0, INFTY, INFTY,    0,INFTY,INFTY),
    "sr-branching-bisimulation"->     WeakObservationNotion(INFTY, INFTY, INFTY, INFTY, INFTY, INFTY,INFTY,INFTY,INFTY)
  )

  def isStabilityCheck(f: HML.Formula[_]) = f match {
    case HML.Negate(HML.ObserveInternal(HML.Pass(HML.And(subs)), false)) =>
      subs.isEmpty
    case HML.Negate(HML.ObserveInternal(HML.And(subs), false)) =>
      subs.isEmpty
    case _ => false
  }

  val LTBTS = Spectrum.fromTuples(BaseLTBTS, getFormulaRootNotion)

  def formulaObsNotion(f: HML.Formula[_]): WeakObservationNotion = f match {
    case HML.And(subterms) =>
      if (subterms.isEmpty) {
        WeakObservationNotion()
      } else if (subterms.forall(isStabilityCheck(_))) {
        WeakObservationNotion(immediateConjunctionLevels = 1, stableConjunctionLevels = 1, negationLevels = 1)
      } else {
        val (positiveSubterms, negativeSubterms) = subterms.toList.partition(_.isPositive)
        val (stabilityChecks, properNegatives) = negativeSubterms.partition(isStabilityCheck(_))
        val positiveClasses = positiveSubterms.map(formulaObsNotion(_))
        val negativeClasses = properNegatives.map(formulaObsNotion(_)) ++ stabilityChecks.map(_ => WeakObservationNotion(0,0,0,0,0,0,0,0,1))
        val allClasses = positiveClasses ++ negativeClasses
        val positiveHeights = positiveClasses.map(_.observationHeight)
        val positiveMaxHeight = if (positiveHeights.isEmpty) 0 else positiveHeights.max
        val positiveHeightSecondary =
          if (stabilityChecks.nonEmpty)
            if (positiveHeights.length <= 1) 0 else (positiveHeights.diff(List(positiveMaxHeight))).max
          else
            0
        val isBranchingObs = positiveSubterms.count {
          case HML.Observe(_, _) | HML.ObserveInternal(_, _) => true
          case _ => false
        }

        // if (allClasses.isEmpty || negativeDeep.nonEmpty || positiveDeep.size > 1) {
          WeakObservationNotion(
            observationHeight = allClasses.map(_.observationHeight).max,
            branchingConjunctionLevels = allClasses.map(_.branchingConjunctionLevels).max + (if (isBranchingObs > 0) 1 else 0),
            unstableConjunctionLevels = allClasses.map(_.unstableConjunctionLevels).max + (if (stabilityChecks.nonEmpty) 0 else 1),
            stableConjunctionLevels = allClasses.map(_.stableConjunctionLevels).max + (if (stabilityChecks.nonEmpty) 1 else 0),
            immediateConjunctionLevels = allClasses.map(_.immediateConjunctionLevels).max + 1,
            positiveConjHeight = (allClasses.map(_.positiveConjHeight) :+ positiveMaxHeight).max,
            positiveConjSecondaryHeight = (allClasses.map(_.positiveConjSecondaryHeight) :+ positiveHeightSecondary).max,
            negativeConjHeight = (allClasses.map(_.negativeConjHeight) ++ negativeClasses.map(_.observationHeight)).max,
            negationLevels = (allClasses.map(_.negationLevels)).max,
          )
        // } else {
        //   // this conjunction can be understood as a local observation
        //   val revivalDepth = (positiveDeep.headOption orElse positiveFlat.headOption).map(_.observationHeight).getOrElse(0)
        //   WeakObservationNotion(
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
    case HML.Negate(andThen) =>
      val andThenClass = formulaObsNotion(andThen)
      WeakObservationNotion(
        negationLevels = andThenClass.negationLevels + 1
      ) lub andThenClass
    case HML.Observe(action, andThen) =>
      val andThenClass = formulaObsNotion(andThen)
      WeakObservationNotion(
        observationHeight = andThenClass.observationHeight + 1
      ) lub andThenClass
    case HML.ObserveInternal(andThen, opt) =>
      val andThenClass = formulaObsNotion(andThen)
      WeakObservationNotion(
        observationHeight = andThenClass.observationHeight + 1
      ) lub andThenClass
    case HML.Pass(andThen) =>
      val andThenClass = formulaObsNotion(andThen)
      if (andThen.isInstanceOf[HML.And[_]]) {
        WeakObservationNotion(
          observationHeight = andThenClass.observationHeight,
          branchingConjunctionLevels = andThenClass.branchingConjunctionLevels,
          unstableConjunctionLevels = andThenClass.unstableConjunctionLevels,
          stableConjunctionLevels = andThenClass.stableConjunctionLevels,
          immediateConjunctionLevels = andThenClass.immediateConjunctionLevels -
            (if (andThenClass.observationHeight <= 0 && andThenClass.stableConjunctionLevels <= 0) 0 else 1), // decreasing!
          positiveConjHeight = andThenClass.positiveConjHeight,
          positiveConjSecondaryHeight = andThenClass.positiveConjSecondaryHeight,
          negativeConjHeight = andThenClass.negativeConjHeight,
          negationLevels = andThenClass.negationLevels,
        )
      } else {
        andThenClass
      }
  }

  def getFormulaRootNotion(f: HML.Formula[_]) = {
    if (f.isPositive) {
      formulaObsNotion(f)
    } else {
      formulaObsNotion(HML.And(Set(f)))
    }
  }

}
