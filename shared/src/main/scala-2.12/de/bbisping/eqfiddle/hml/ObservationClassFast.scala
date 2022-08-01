package de.bbisping.eqfiddle.hml

case class ObservationClassFast(
  /** the maximal observation depth of the subformulas (⊤ has height 0, negation and conjunction are neutral wrt. height) */
  observationHeight: Int = 0,
  /** the maximal amount of conjunctions when descending into a formula */
  conjunctionLevels: Int = 0,
  /** the maximal height of positive branches (observationHeight)*/
  maxPositiveConjunctHeight: Int = 0,
  /** the maximal height of negative branches (observationHeight)*/
  maxNegativeConjunctHeight: Int = 0
) extends ObservationClass {

  override def tryCompareTo[B >: ObservationClass](that: B)(implicit evidence$1: B => PartiallyOrdered[B]): Option[Int] = {
    that match {
      case that: ObservationClassFast =>
        if (this == that) {
          Some(0)
        } else if (
            this.observationHeight >= that.observationHeight &&
            this.conjunctionLevels >= that.conjunctionLevels &&
            this.maxPositiveConjunctHeight >= that.maxPositiveConjunctHeight &&
            this.maxNegativeConjunctHeight >= that.maxNegativeConjunctHeight) {
          Some(1)
        } else if (
            this.observationHeight <= that.observationHeight &&
            this.conjunctionLevels <= that.conjunctionLevels &&
            this.maxPositiveConjunctHeight <= that.maxPositiveConjunctHeight &&
            this.maxNegativeConjunctHeight <= that.maxNegativeConjunctHeight) {
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
        Integer.max(this.maxPositiveConjunctHeight, that.maxPositiveConjunctHeight),
        Integer.max(this.maxNegativeConjunctHeight, that.maxNegativeConjunctHeight)
      )
    case _ => this
  }

  override def glb[B >: ObservationClassFast](that: B): B = that match {
    case that: ObservationClassFast =>
      ObservationClassFast(
        Integer.min(this.observationHeight, that.observationHeight),
        Integer.min(this.conjunctionLevels, that.conjunctionLevels),
        Integer.min(this.maxPositiveConjunctHeight, that.maxPositiveConjunctHeight),
        Integer.min(this.maxNegativeConjunctHeight, that.maxNegativeConjunctHeight)
      )
    case _ => this
  }

  override def toTuple = (observationHeight, conjunctionLevels, maxPositiveConjunctHeight, maxNegativeConjunctHeight)
}

object ObservationClassFast {
  val INFTY = Integer.MAX_VALUE

  // observationHeight, conjunctionLevels, maxPosHeight, maxNegHeight
  // The Linear-time Branching-time Spectrum
  val BaseLTBTS = List(
    "enabledness" ->        ObservationClassFast(    1,     0,    0,    0),
    "traces" ->             ObservationClassFast(INFTY,     0,    0,    0),
    "failure" ->            ObservationClassFast(INFTY,     1,    0,    1),
    "readiness" ->          ObservationClassFast(INFTY,     1,    1,    1),
    "impossible-future" ->  ObservationClassFast(INFTY,     1,    0,INFTY),
    "possible-future" ->    ObservationClassFast(INFTY,     1,INFTY,INFTY),
    "simulation" ->         ObservationClassFast(INFTY, INFTY,INFTY,    0),
    "ready-simulation" ->   ObservationClassFast(INFTY, INFTY,INFTY,    1),
    "bisimulation" ->       ObservationClassFast(INFTY, INFTY,INFTY,INFTY)
  )

  val LTBTS = Spectrum.fromTuples(BaseLTBTS)


  implicit class FastClassifiedFormula[A](formula: HennessyMilnerLogic.Formula[A]) extends ObservationClass.ClassifiedFormula[A] {

    override def isImmediate = formula.isImmediate

    override def isPositive = formula.isPositive

    override def obsClass: ObservationClassFast = ObservationClassFast()

    override def getRootClass() = obsClass

  }

}
