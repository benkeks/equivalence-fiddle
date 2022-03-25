package de.bbisping.eqfiddle.hml

case class ObservationClass(
  /** the maximal observation depth of the subformulas (⊤ has height 0, negation and conjunction are neutral wrt. height) */
  observationHeight: Int,
  /** the maximal amount of conjunctions when descending into a formula */
  conjunctionLevels: Int,
  /** the maximal amount of positive deep branches (observationHeight > 1)*/
  maxPositiveDeepBranches: Int,
  /** the maximal amount of positive branches*/
  maxPositiveBranches: Int,
  /** the maximal amount of negations when descending into a formula */
  negationLevels: Int,
  /** maximal observationHeight of negative subformulas */
  maxNegationHeight: Int
) {
  def lub(that: ObservationClass) = ObservationClass(
    Integer.max(this.observationHeight, that.observationHeight),
    Integer.max(this.conjunctionLevels, that.conjunctionLevels),
    Integer.max(this.maxPositiveDeepBranches, that.maxPositiveDeepBranches),
    Integer.max(this.maxPositiveBranches, that.maxPositiveBranches),
    Integer.max(this.negationLevels, that.negationLevels),
    Integer.max(this.maxNegationHeight, that.maxNegationHeight)
  )

  def glb(that: ObservationClass) = ObservationClass(
    Integer.min(this.observationHeight, that.observationHeight),
    Integer.min(this.conjunctionLevels, that.conjunctionLevels),
    Integer.min(this.maxPositiveDeepBranches, that.maxPositiveDeepBranches),
    Integer.min(this.maxPositiveBranches, that.maxPositiveBranches),
    Integer.min(this.negationLevels, that.negationLevels),
    Integer.min(this.maxNegationHeight, that.maxNegationHeight)
  )

  def above(that: ObservationClass) = (
    this.observationHeight >= that.observationHeight &&
    this.conjunctionLevels >= that.conjunctionLevels &&
    this.maxPositiveDeepBranches >= that.maxPositiveDeepBranches &&
    this.maxPositiveBranches >= that.maxPositiveBranches &&
    this.negationLevels >= that.negationLevels &&
    this.maxNegationHeight >= that.maxNegationHeight
  )

  def strictlyAbove(that: ObservationClass) = (this != that) && (this above that)

  def below(that: ObservationClass) = (
    this.observationHeight <= that.observationHeight &&
    this.conjunctionLevels <= that.conjunctionLevels &&
    this.maxPositiveDeepBranches <= that.maxPositiveDeepBranches &&
    this.maxPositiveBranches <= that.maxPositiveBranches &&
    this.negationLevels <= that.negationLevels &&
    this.maxNegationHeight <= that.maxNegationHeight
  )

  def strictlyBelow(that: ObservationClass) = (this != that) && (this below that)
}

object ObservationClass {
  val INFTY = Integer.MAX_VALUE

  type EquivalenceNotion = (String, ObservationClass)

  // observationHeight, conjunctionLevels, maxPosDeep, maxPos, negationLevels, maxNegH
  // The Linear-time Branching-time Spectrum
  val LTBTS = List(
    "enabledness" ->        ObservationClass(    1,     0,    0,    0,    0,    0),
    "traces" ->             ObservationClass(INFTY,     0,    0,    0,    0,    0),
    "failure" ->            ObservationClass(INFTY,     1,    0,    0,    1,    1),
    "readiness" ->          ObservationClass(INFTY,     1,    0,INFTY,    1,    1),
    "failure-trace" ->      ObservationClass(INFTY, INFTY,    1,    1,    1,    1),
    "ready-trace" ->        ObservationClass(INFTY, INFTY,    1,INFTY,    1,    1),
    "impossible-future" ->  ObservationClass(INFTY,     1,    0,    0,    1,INFTY),
    "possible-future" ->    ObservationClass(INFTY,     1,INFTY,INFTY,    1,INFTY),
    "simulation" ->         ObservationClass(INFTY, INFTY,INFTY,INFTY,    0,    0),
    "ready-simulation" ->   ObservationClass(INFTY, INFTY,INFTY,INFTY,    1,    1),
    "2-nested-simulation"-> ObservationClass(INFTY, INFTY,INFTY,INFTY,    1,INFTY),
    "bisimulation" ->       ObservationClass(INFTY, INFTY,INFTY,INFTY,INFTY,INFTY)
  )

  val LTBTSNotionNames = LTBTS.map(_._1).toSet

  def getSpectrumClass(name: String) = LTBTS.find(_._1 == name)

  /** given a group of least distinguishing observation classes, tell what weaker ObservationClasses would be the strongest fit to preorder the distinguished states */
  def getStrongestPreorderClass[A](leastClassifications: Iterable[(String, ObservationClass)]): List[(String, ObservationClass)] = {
    
    val weakerClasses = LTBTS.filterNot { c => leastClassifications.exists(c._2 above _._2) }
    val mostFitting = weakerClasses.filterNot { c => weakerClasses.exists(_._2 strictlyAbove c._2) }

    mostFitting.toList
  }
}
  