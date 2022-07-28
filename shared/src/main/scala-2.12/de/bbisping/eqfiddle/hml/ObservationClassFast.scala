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
) {
  def lub(that: ObservationClassFast) = ObservationClassFast(
    Integer.max(this.observationHeight, that.observationHeight),
    Integer.max(this.conjunctionLevels, that.conjunctionLevels),
    Integer.max(this.maxPositiveConjunctHeight, that.maxPositiveConjunctHeight),
    Integer.max(this.maxNegativeConjunctHeight, that.maxNegativeConjunctHeight)
  )

  def glb(that: ObservationClassFast) = ObservationClassFast(
    Integer.min(this.observationHeight, that.observationHeight),
    Integer.min(this.conjunctionLevels, that.conjunctionLevels),
    Integer.min(this.maxPositiveConjunctHeight, that.maxPositiveConjunctHeight),
    Integer.min(this.maxNegativeConjunctHeight, that.maxNegativeConjunctHeight)
  )

  def above(that: ObservationClassFast) = (
    this.observationHeight >= that.observationHeight &&
    this.conjunctionLevels >= that.conjunctionLevels &&
    this.maxPositiveConjunctHeight >= that.maxPositiveConjunctHeight &&
    this.maxNegativeConjunctHeight >= that.maxNegativeConjunctHeight
  )

  def strictlyAbove(that: ObservationClassFast) = (this != that) && (this above that)

  def below(that: ObservationClassFast) = (
    this.observationHeight <= that.observationHeight &&
    this.conjunctionLevels <= that.conjunctionLevels &&
    this.maxPositiveConjunctHeight <= that.maxPositiveConjunctHeight &&
    this.maxNegativeConjunctHeight <= that.maxNegativeConjunctHeight
  )

  def strictlyBelow(that: ObservationClassFast) = (this != that) && (this below that)

  def toTuple = (observationHeight, conjunctionLevels, maxPositiveConjunctHeight, maxNegativeConjunctHeight)
}

object ObservationClassFast {
  val INFTY = Integer.MAX_VALUE

  type EquivalenceNotion = (String, ObservationClassFast)

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

  val LTBTS = BaseLTBTS

  val LTBTSNotionNames = LTBTS.map(_._1).toSet

  def getSpectrumClass(name: String) = LTBTS.find(_._1 == name)

  /** given a group of least distinguishing observation classes, tell what weaker ObservationClasses would be the strongest fit to preorder the distinguished states */
  def getStrongestPreorderClass[A](leastClassifications: Iterable[(String, ObservationClassFast)]): List[(String, ObservationClassFast)] = {

    val weakerClasses = LTBTS.filterNot { c => leastClassifications.exists(c._2 above _._2) }
    val mostFitting = weakerClasses.filterNot { c => weakerClasses.exists(_._2 strictlyAbove c._2) }

    mostFitting.toList
  }
}
  