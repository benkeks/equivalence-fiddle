package de.bbisping.eqfiddle.hml

case class ObservationClass(
  /** the maximal observation depth of the subformulas (⊤ has height 0, negation and conjunction are neutral wrt. height) */
  observationHeight: Int,
  /** the maximal amount of conjunctions when descending into a formula */
  conjunctionLevels: Int,
  /** the maximal amount of negations when descending into a formula */
  negationLevels: Int,
  /** the maximal amount of positive deep branches (observationHeight > 1)*/
  maxPositiveDeepBranches: Int,
  /** the maximal amount of positive branches*/
  maxPositiveBranches: Int,
  /** maximal observationHeight of negative subformulas */
  maxNegationHeight: Int,
  /** if there are any conjunctions with positive subformulas */
  nonNegativeConjuncts: Boolean
) {
  def lub(that: ObservationClass) = ObservationClass(
    Integer.max(this.observationHeight, that.observationHeight),
    Integer.max(this.conjunctionLevels, that.conjunctionLevels),
    Integer.max(this.negationLevels, that.negationLevels),
    Integer.max(this.maxPositiveDeepBranches, that.maxPositiveDeepBranches),
    Integer.max(this.maxPositiveBranches, that.maxPositiveBranches),
    Integer.max(this.maxNegationHeight, that.maxNegationHeight),
    this.nonNegativeConjuncts || that.nonNegativeConjuncts
  )

  def glb(that: ObservationClass) = ObservationClass(
    Integer.min(this.observationHeight, that.observationHeight),
    Integer.min(this.conjunctionLevels, that.conjunctionLevels),
    Integer.min(this.negationLevels, that.negationLevels),
    Integer.min(this.maxPositiveDeepBranches, that.maxPositiveDeepBranches),
    Integer.min(this.maxPositiveBranches, that.maxPositiveBranches),
    Integer.min(this.maxNegationHeight, that.maxNegationHeight),
    this.nonNegativeConjuncts && that.nonNegativeConjuncts
  )

  def above(that: ObservationClass) = (
    this.observationHeight >= that.observationHeight &&
    this.conjunctionLevels >= that.conjunctionLevels &&
    this.negationLevels >= that.negationLevels &&
    this.maxPositiveDeepBranches >= that.maxPositiveDeepBranches &&
    this.maxPositiveBranches >= that.maxPositiveBranches &&
    this.maxNegationHeight >= that.maxNegationHeight &&
    (this.nonNegativeConjuncts || !that.nonNegativeConjuncts)
  )

  def strictlyAbove(that: ObservationClass) = (this != that) && (this above that)

  def below(that: ObservationClass) = (
    this.observationHeight <= that.observationHeight &&
    this.conjunctionLevels <= that.conjunctionLevels &&
    this.negationLevels <= that.negationLevels &&
    this.maxPositiveDeepBranches <= that.maxPositiveDeepBranches &&
    this.maxPositiveBranches <= that.maxPositiveBranches &&
    this.maxNegationHeight <= that.maxNegationHeight &&
    (!this.nonNegativeConjuncts || that.nonNegativeConjuncts)
  )

  def strictlyBelow(that: ObservationClass) = (this != that) && (this below that)
}

object ObservationClass {
  val INFTY = Integer.MAX_VALUE

  // observationHeight, conjunctionLevels, negationLevels, maxPosDeep, maxNegDeep, maxPosFlat, maxNegH, nonNegConjs
  // nonNegConjs is necessary, because maxPosFlat will sometimes count one positive flat branch as deep to account for trace equivalences. 
  // The Linear-time Branching-time Spectrum
  val LTBTS = List(
    "enabledness" ->        ObservationClass(    1,     0,    0,    0,    0,    0,false),
    "traces" ->             ObservationClass(INFTY,     0,    0,    0,    0,    0,false),
    "failure" ->            ObservationClass(INFTY,     1,    1,    0,    0,    1,false),
    "readiness" ->          ObservationClass(INFTY,     1,    1,    0,INFTY,    1,true),
    "failure-trace" ->      ObservationClass(INFTY, INFTY,    1,    1,    1,    1,true),
    "ready-trace" ->        ObservationClass(INFTY, INFTY,    1,    1,INFTY,    1,true),
    "impossible-future" ->  ObservationClass(INFTY,     1,    1,    0,    0,INFTY,false),
    "possible-future" ->    ObservationClass(INFTY,     1,    1,INFTY,INFTY,INFTY,true),
    "simulation" ->         ObservationClass(INFTY, INFTY,    0,INFTY,INFTY,    0,true),
    "ready-simulation" ->   ObservationClass(INFTY, INFTY,    1,INFTY,INFTY,    1,true),
    "2-nested-simulation"-> ObservationClass(INFTY, INFTY,    1,INFTY,INFTY,INFTY,true),
    "bisimulation" ->       ObservationClass(INFTY, INFTY,INFTY,INFTY,INFTY,INFTY,true)
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
  