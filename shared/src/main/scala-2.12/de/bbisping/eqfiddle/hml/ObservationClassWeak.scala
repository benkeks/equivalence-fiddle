package de.bbisping.eqfiddle.hml
import de.bbisping.eqfiddle.hml.HennessyMilnerLogic.And
import de.bbisping.eqfiddle.hml.HennessyMilnerLogic.Negate
import de.bbisping.eqfiddle.hml.HennessyMilnerLogic.Observe
import de.bbisping.eqfiddle.hml.HennessyMilnerLogic.Pass

case class ObservationClassWeak(
  /** the maximal observation depth of the subformulas (⊤ has height 0, negation and conjunction are neutral wrt. height) */
  observationHeight: Int = 0,
  /** the maximal amount of conjunctions when descending into a formula */
  conjunctionLevels: Int = 0,
  /** the maximal amount of positive deep branches (observationHeight > 1)*/
  maxPositiveDeepBranches: Int = 0,
  /** the maximal amount of positive branches*/
  maxPositiveBranches: Int = 0,
  /** the maximal amount of negations when descending into a formula */
  negationLevels: Int = 0,
  /** maximal observationHeight of negative subformulas */
  maxNegatedHeight: Int = 0,
  /** are there immediate conjunctions / conjunctions with immediate clauses (2 if both) */
  immediateConjunctions: Int = 0,
  /** how many immediate observation clauses may appear in a conjunction */
  immediateClauses: Int = 0
) extends ObservationClass {

  override def tryCompareTo[B >: ObservationClass](that: B)(implicit evidence$1: B => PartiallyOrdered[B]): Option[Int] = {
    that match {
      case that: ObservationClassWeak =>
        if (this == that) {
          Some(0)
        } else if (
            this.observationHeight >= that.observationHeight &&
            this.conjunctionLevels >= that.conjunctionLevels &&
            this.maxPositiveDeepBranches >= that.maxPositiveDeepBranches &&
            this.maxPositiveBranches >= that.maxPositiveBranches &&
            this.negationLevels >= that.negationLevels &&
            this.maxNegatedHeight >= that.maxNegatedHeight &&
            this.immediateConjunctions >= that.immediateConjunctions &&
            this.immediateClauses >= that.immediateClauses) {
          Some(1)
        } else if (
            this.observationHeight <= that.observationHeight &&
            this.conjunctionLevels <= that.conjunctionLevels &&
            this.maxPositiveDeepBranches <= that.maxPositiveDeepBranches &&
            this.maxPositiveBranches <= that.maxPositiveBranches &&
            this.negationLevels <= that.negationLevels &&
            this.maxNegatedHeight <= that.maxNegatedHeight &&
            this.immediateConjunctions <= that.immediateConjunctions &&
            this.immediateClauses <= that.immediateClauses) {
          Some(-1)
        } else {
          None
        }
      case _ => None
    }
  }

  override def lub[B >: ObservationClassWeak](that: B): B = that match {
    case that: ObservationClassWeak =>
      ObservationClassWeak(
        Integer.max(this.observationHeight, that.observationHeight),
        Integer.max(this.conjunctionLevels, that.conjunctionLevels),
        Integer.max(this.maxPositiveDeepBranches, that.maxPositiveDeepBranches),
        Integer.max(this.maxPositiveBranches, that.maxPositiveBranches),
        Integer.max(this.negationLevels, that.negationLevels),
        Integer.max(this.maxNegatedHeight, that.maxNegatedHeight),
        Integer.max(this.immediateConjunctions, that.immediateConjunctions),
        Integer.max(this.immediateClauses, that.immediateClauses)
      )
    case _ => this
  }

  override def glb[B >: ObservationClass](that: B): B = that match {
    case that: ObservationClassWeak =>
      ObservationClassWeak(
        Integer.min(this.observationHeight, that.observationHeight),
        Integer.min(this.conjunctionLevels, that.conjunctionLevels),
        Integer.min(this.maxPositiveDeepBranches, that.maxPositiveDeepBranches),
        Integer.min(this.maxPositiveBranches, that.maxPositiveBranches),
        Integer.min(this.negationLevels, that.negationLevels),
        Integer.min(this.maxNegatedHeight, that.maxNegatedHeight),
        Integer.min(this.immediateConjunctions, that.immediateConjunctions),
        Integer.min(this.immediateClauses, that.immediateClauses)
      )
    case _ => this
  }

  override def toTuple = (observationHeight, conjunctionLevels, maxPositiveDeepBranches, maxPositiveBranches, negationLevels, maxNegatedHeight, immediateConjunctions, immediateClauses)
}

object ObservationClassWeak {
  val INFTY = Integer.MAX_VALUE

  type EquivalenceNotion = (String, ObservationClass)

  // observationHeight, conjunctionLevels, maxPosDeep, maxPos, negationLevels, maxNegH
  // The Linear-time Branching-time Spectrum
  val BaseLTBTS = List(
    "enabledness" ->        ObservationClassWeak(    1,     0,    0,    0,    0,    0),
    "traces" ->             ObservationClassWeak(INFTY,     0,    0,    0,    0,    0),
    "failure" ->            ObservationClassWeak(INFTY,     1,    0,    0,    1,    1),
    "readiness" ->          ObservationClassWeak(INFTY,     1,    0,INFTY,    1,    1),
    "failure-trace" ->      ObservationClassWeak(INFTY, INFTY,    1,    1,    1,    1),
    "ready-trace" ->        ObservationClassWeak(INFTY, INFTY,    1,INFTY,    1,    1),
    "impossible-future" ->  ObservationClassWeak(INFTY,     1,    0,    0,    1,INFTY),
    "possible-future" ->    ObservationClassWeak(INFTY,     1,INFTY,INFTY,    1,INFTY),
    "simulation" ->         ObservationClassWeak(INFTY, INFTY,INFTY,INFTY,    0,    0),
    "ready-simulation" ->   ObservationClassWeak(INFTY, INFTY,INFTY,INFTY,    1,    1),
    "2-nested-simulation"-> ObservationClassWeak(INFTY, INFTY,INFTY,INFTY,    1,INFTY),
    "bisimulation" ->       ObservationClassWeak(INFTY, INFTY,INFTY,INFTY,INFTY,INFTY)
  )

  val StrongLTBTS = BaseLTBTS map {
    case (name, obsClass) => ("strong-"+name, obsClass lub ObservationClassWeak(immediateConjunctions = INFTY, immediateClauses = INFTY))
  }

  val WeakLTBTS = BaseLTBTS map {
    case (name, obsClass) => ("weak-"+name, obsClass)
  }

  val SpecialWeakEqs = List(
    "contrasimulation"->       ObservationClassWeak(INFTY, INFTY,    0,    0,INFTY,INFTY,0,0),
    "delay-bisimulation"->     ObservationClassWeak(INFTY, INFTY,INFTY,INFTY,INFTY,INFTY,1,0),
    "eta-bisimulation"->       ObservationClassWeak(INFTY, INFTY,INFTY,INFTY,INFTY,INFTY,0,1),
    "branching-bisimulation"-> ObservationClassWeak(INFTY, INFTY,INFTY,INFTY,INFTY,INFTY,1,1),
    "strong-bisimulation" ->   ObservationClassWeak(INFTY, INFTY,INFTY,INFTY,INFTY,INFTY,INFTY,INFTY)
  )

  val LTBTS = Spectrum.fromTuples(WeakLTBTS ++ SpecialWeakEqs)

  implicit class WeaklyClassifiedFormula[A](formula: HennessyMilnerLogic.Formula[A]) extends ObservationClass.ClassifiedFormula[A] {

    override def isImmediate = formula.isImmediate

    override def isPositive = formula.isPositive

    override def obsClass: ObservationClassWeak = formula match {
      case And(subterms) =>
        if (subterms.isEmpty) {
          ObservationClassWeak()
        } else {
          val positiveSubterms = subterms.filter(_.isPositive)
          val positiveFlatCount = positiveSubterms.count(_.obsClass.observationHeight <= 1)
          val immediateClauseCount = subterms.count(_.isImmediate)

          ObservationClassWeak(
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
            maxNegatedHeight = subterms.map(_.obsClass.maxNegatedHeight).max,
            immediateConjunctions = (subterms.map(_.obsClass.immediateConjunctions)).max,
            immediateClauses = (subterms.map(_.obsClass.immediateClauses) + immediateClauseCount).max
          )
        }
      case Negate(andThen) =>
        val andThenClass = andThen.obsClass.asInstanceOf[ObservationClassWeak]
        ObservationClassWeak(
          conjunctionLevels = andThenClass.conjunctionLevels + (if (!andThen.isPositive) 1 else 0),
          negationLevels = andThenClass.negationLevels + 1,
          maxNegatedHeight = andThenClass.observationHeight
        ) lub andThenClass
      case Observe(action, andThen) =>
        val andThenClass = andThen.obsClass.asInstanceOf[ObservationClassWeak]
        ObservationClassWeak(
          observationHeight = andThenClass.observationHeight + 1,
          conjunctionLevels = if (!andThen.isPositive) andThenClass.conjunctionLevels + 1 else 0,
          immediateConjunctions = andThen match { case Observe(_, _) => 2; case And(subs) if subs.exists(_.isImmediate) => 2; case _ if andThen.isImmediate => 1; case _ => 0},
        ) lub andThenClass
      case Pass(andThen) =>
        andThen.obsClass
    }

    /** class of this formula if it appears at the top level */
    override def getRootClass() = {
      ObservationClassWeak(
        conjunctionLevels = obsClass.asInstanceOf[ObservationClassWeak].conjunctionLevels + (if (!isPositive) 1 else 0),
        immediateConjunctions = formula match { case HennessyMilnerLogic.Observe(_, _) => 2; case _ if formula.isImmediate => 1; case _ => 0},
        immediateClauses = if (formula.isImmediate) 1 else 0
      ) lub obsClass
    }

  }
}
