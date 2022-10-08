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
    "2traces" ->            ObservationClassFast(    2,     0,    0,    0),
    "2failure" ->           ObservationClassFast(    2,     1,    0,    1),
    "traces" ->             ObservationClassFast(INFTY,     0,    0,    0),
    "failure" ->            ObservationClassFast(INFTY,     1,    0,    1),
    "readiness" ->          ObservationClassFast(INFTY,     1,    1,    1),
    "impossible-future" ->  ObservationClassFast(INFTY,     1,    0,INFTY),
    "possible-future" ->    ObservationClassFast(INFTY,     1,INFTY,INFTY),
    "simulation" ->         ObservationClassFast(INFTY, INFTY,INFTY,    0),
    "ready-simulation" ->   ObservationClassFast(INFTY, INFTY,INFTY,    1),
    "bisimulation" ->       ObservationClassFast(INFTY, INFTY,INFTY,INFTY)
  )

  val LTBTS = Spectrum.fromTuples(BaseLTBTS, getFormulaRootClass)

  def formulaObsClass(f: HennessyMilnerLogic.Formula[_]): ObservationClassFast = f match {
    case HennessyMilnerLogic.And(subterms) =>
      if (subterms.isEmpty) {
        ObservationClassFast()
      } else {
        val (positiveSubterms, negativeSubterms) = subterms.partition(_.isPositive)
        val subtermObsClass = subterms.map(formulaObsClass(_))

        ObservationClassFast(
          observationHeight = subtermObsClass.map(_.observationHeight).max,
          conjunctionLevels = subtermObsClass.map(_.conjunctionLevels).max + 1,
          maxPositiveConjunctHeight =
            (positiveSubterms.map(formulaObsClass(_).observationHeight) ++ subtermObsClass.map(_.maxPositiveConjunctHeight)).max,
          maxNegativeConjunctHeight =
            (negativeSubterms.map(formulaObsClass(_).observationHeight) ++ subtermObsClass.map(_.maxNegativeConjunctHeight)).max
        )
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
