package io.equiv.eqfiddle.hml

case class ObservationNotionStrong(
  /** the maximal observation depth of the subformulas (⊤ has height 0, negation and conjunction are neutral wrt. height) */
  observationHeight: Int = 0,
  /** the maximal amount of conjunctions when descending into a formula */
  conjunctionLevels: Int = 0,
  /** the maximal height of revivals at local-test conjunctions (observationHeight)*/
  revivalHeight: Int = 0,
  /** maximal height of positive conjunctions (observationHeight) excluding revivals*/
  positiveConjHeight: Int = 0,
  /** maximal height of negative conjuncts */
  negativeConjHeight: Int = 0,
  /** how many negation levels may happen? */
  negationLevels: Int = 0
) extends ObservationNotion {

  override def tryCompareTo[B >: ObservationNotion](that: B)(implicit evidence$1: B => PartiallyOrdered[B]): Option[Int] = {
    that match {
      case that: ObservationNotionStrong =>
        if (this == that) {
          Some(0)
        } else if (
            this.observationHeight >= that.observationHeight &&
            this.conjunctionLevels >= that.conjunctionLevels &&
            this.revivalHeight >= that.revivalHeight &&
            this.positiveConjHeight >= that.positiveConjHeight &&
            this.negativeConjHeight >= that.negativeConjHeight &&
            this.negationLevels >= that.negationLevels) {
          Some(1)
        } else if (
            this.observationHeight <= that.observationHeight &&
            this.conjunctionLevels <= that.conjunctionLevels &&
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

  override def lub[B >: ObservationNotionStrong](that: B): B = that match {
    case that: ObservationNotionStrong =>
      ObservationNotionStrong(
        Integer.max(this.observationHeight, that.observationHeight),
        Integer.max(this.conjunctionLevels, that.conjunctionLevels),
        Integer.max(this.revivalHeight, that.revivalHeight),
        Integer.max(this.positiveConjHeight, that.positiveConjHeight),
        Integer.max(this.negativeConjHeight, that.negativeConjHeight),
        Integer.max(this.negationLevels, that.negationLevels)
      )
    case _ => this
  }

  override def glb[B >: ObservationNotionStrong](that: B): B = that match {
    case that: ObservationNotionStrong =>
      ObservationNotionStrong(
        Integer.min(this.observationHeight, that.observationHeight),
        Integer.min(this.conjunctionLevels, that.conjunctionLevels),
        Integer.min(this.revivalHeight, that.revivalHeight),
        Integer.min(this.positiveConjHeight, that.positiveConjHeight),
        Integer.min(this.negativeConjHeight, that.negativeConjHeight),
        Integer.min(this.negationLevels, that.negationLevels)
      )
    case _ => this
  }

  override def toTuple = (observationHeight, conjunctionLevels, revivalHeight, positiveConjHeight, negativeConjHeight, negationLevels)
}

object ObservationNotionStrong {
  val INFTY = Integer.MAX_VALUE

  // observationHeight, conjunctionLevels, revivalHeight, positiveConjHeight, negativeConjHeight, negationLevels
  // The Linear-time Branching-time Spectrum
  val BaseLTBTS = List(
    //"universal" ->          ObservationNotionStrong(    0,     0,    0,    0,    0,    0), // left out to declutter output
    "enabledness" ->        ObservationNotionStrong(    1,     0,    0,    0,    0,    0),
    "trace" ->              ObservationNotionStrong(INFTY,     0,    0,    0,    0,    0),
    "failure" ->            ObservationNotionStrong(INFTY,     1,    0,    0,    1,    1),
    "revivals" ->           ObservationNotionStrong(INFTY,     1,    1,    0,    1,    1),
    "readiness" ->          ObservationNotionStrong(INFTY,     1,    1,    1,    1,    1),
    "failure-trace" ->      ObservationNotionStrong(INFTY, INFTY,INFTY,    0,    1,    1),
    "ready-trace" ->        ObservationNotionStrong(INFTY, INFTY,INFTY,    1,    1,    1),
    "impossible-future" ->  ObservationNotionStrong(INFTY,     1,    0,    0,INFTY,    1),
    "possible-future" ->    ObservationNotionStrong(INFTY,     1,INFTY,INFTY,INFTY,    1),
    "simulation" ->         ObservationNotionStrong(INFTY, INFTY,INFTY,INFTY,    0,    0),
    "ready-simulation" ->   ObservationNotionStrong(INFTY, INFTY,INFTY,INFTY,    1,    1),
    "2-nested-simulation"-> ObservationNotionStrong(INFTY, INFTY,INFTY,INFTY,INFTY,    1),
    "bisimulation" ->       ObservationNotionStrong(INFTY, INFTY,INFTY,INFTY,INFTY,INFTY)
  )


  val LTBTS = Spectrum.fromTuples(BaseLTBTS, getFormulaRootClass)

  def formulaobsNotion(f: HennessyMilnerLogic.Formula[_]): ObservationNotionStrong = f match {
    case HennessyMilnerLogic.And(subterms) =>
      if (subterms.isEmpty) {
        ObservationNotionStrong()
      } else {
        val (positiveSubterms, negativeSubterms) = subterms.toList.partition(_.isPositive)
        val positiveClasses = positiveSubterms.map(formulaobsNotion(_))
        val negativeClasses = negativeSubterms.map(formulaobsNotion(_))
        val revivalClass = if (positiveClasses.nonEmpty) Some(positiveClasses.maxBy(_.observationHeight)) else None
        val otherPositiveClasses = positiveClasses diff revivalClass.toList
        val allClasses = positiveClasses ++ negativeClasses

        ObservationNotionStrong(
          observationHeight = allClasses.map(_.observationHeight).max,
          conjunctionLevels = allClasses.map(_.conjunctionLevels).max + 1,
          revivalHeight = Integer.max(revivalClass.map(_.observationHeight).getOrElse(0), allClasses.map(_.revivalHeight).max),
          positiveConjHeight = (allClasses.map(_.positiveConjHeight) ++ otherPositiveClasses.map(_.observationHeight)).max,
          negativeConjHeight = (allClasses.map(_.negativeConjHeight) ++ negativeClasses.map(_.observationHeight)).max,
          negationLevels = allClasses.map(_.negationLevels).max,
        )
      }
    case HennessyMilnerLogic.Negate(andThen) =>
      val andThenClass = formulaobsNotion(andThen)
      ObservationNotionStrong(
        negationLevels = andThenClass.negationLevels + 1
      ) lub andThenClass
    case HennessyMilnerLogic.Observe(action, andThen) =>
      val andThenClass = formulaobsNotion(andThen)
      ObservationNotionStrong(
        observationHeight = andThenClass.observationHeight + 1
      ) lub andThenClass
    case HennessyMilnerLogic.Pass(andThen) =>
      formulaobsNotion(andThen)
  }

  def getFormulaRootClass(f: HennessyMilnerLogic.Formula[_]) = {
    if (f.isPositive) {
      formulaobsNotion(f)
    } else {
      formulaobsNotion(HennessyMilnerLogic.And(Set(f)))
    }
  }

}
