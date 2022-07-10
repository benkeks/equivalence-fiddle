package de.bbisping.eqfiddle.hml

case class ObservationClass(
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
) {
  def lub(that: ObservationClass) = ObservationClass(
    Integer.max(this.observationHeight, that.observationHeight),
    Integer.max(this.conjunctionLevels, that.conjunctionLevels),
    Integer.max(this.maxPositiveDeepBranches, that.maxPositiveDeepBranches),
    Integer.max(this.maxPositiveBranches, that.maxPositiveBranches),
    Integer.max(this.negationLevels, that.negationLevels),
    Integer.max(this.maxNegatedHeight, that.maxNegatedHeight),
    Integer.max(this.immediateConjunctions, that.immediateConjunctions),
    Integer.max(this.immediateClauses, that.immediateClauses)
  )

  def glb(that: ObservationClass) = ObservationClass(
    Integer.min(this.observationHeight, that.observationHeight),
    Integer.min(this.conjunctionLevels, that.conjunctionLevels),
    Integer.min(this.maxPositiveDeepBranches, that.maxPositiveDeepBranches),
    Integer.min(this.maxPositiveBranches, that.maxPositiveBranches),
    Integer.min(this.negationLevels, that.negationLevels),
    Integer.min(this.maxNegatedHeight, that.maxNegatedHeight),
    Integer.min(this.immediateConjunctions, that.immediateConjunctions),
    Integer.min(this.immediateClauses, that.immediateClauses)
  )

  def above(that: ObservationClass) = (
    this.observationHeight >= that.observationHeight &&
    this.conjunctionLevels >= that.conjunctionLevels &&
    this.maxPositiveDeepBranches >= that.maxPositiveDeepBranches &&
    this.maxPositiveBranches >= that.maxPositiveBranches &&
    this.negationLevels >= that.negationLevels &&
    this.maxNegatedHeight >= that.maxNegatedHeight &&
    this.immediateConjunctions >= that.immediateConjunctions &&
    this.immediateClauses >= that.immediateClauses
  )

  def strictlyAbove(that: ObservationClass) = (this != that) && (this above that)

  def below(that: ObservationClass) = (
    this.observationHeight <= that.observationHeight &&
    this.conjunctionLevels <= that.conjunctionLevels &&
    this.maxPositiveDeepBranches <= that.maxPositiveDeepBranches &&
    this.maxPositiveBranches <= that.maxPositiveBranches &&
    this.negationLevels <= that.negationLevels &&
    this.maxNegatedHeight <= that.maxNegatedHeight &&
    this.immediateConjunctions <= that.immediateConjunctions &&
    this.immediateClauses <= that.immediateClauses
  )

  def strictlyBelow(that: ObservationClass) = (this != that) && (this below that)

  def toTuple = (observationHeight, conjunctionLevels, maxPositiveDeepBranches, maxPositiveBranches, negationLevels, maxNegatedHeight, immediateConjunctions, immediateClauses)
}

object ObservationClass {
  val INFTY = Integer.MAX_VALUE

  type EquivalenceNotion = (String, ObservationClass)

  // observationHeight, conjunctionLevels, maxPosDeep, maxPos, negationLevels, maxNegH
  // The Linear-time Branching-time Spectrum
  val BaseLTBTS = List(
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

  val StrongLTBTS = BaseLTBTS map {
    case (name, obsClass) => ("strong-"+name, obsClass lub ObservationClass(immediateConjunctions = INFTY, immediateClauses = INFTY))
  }

  val WeakLTBTS = BaseLTBTS map {
    case (name, obsClass) => ("weak-"+name, obsClass)
  }

  val SpecialWeakEqs = List(
    "contrasimulation"->       ObservationClass(INFTY, INFTY,    0,    0,INFTY,INFTY,0,0),
    "delay-bisimulation"->     ObservationClass(INFTY, INFTY,INFTY,INFTY,INFTY,INFTY,1,0),
    "eta-bisimulation"->       ObservationClass(INFTY, INFTY,INFTY,INFTY,INFTY,INFTY,0,1),
    "branching-bisimulation"-> ObservationClass(INFTY, INFTY,INFTY,INFTY,INFTY,INFTY,1,1),
    "strong-bisimulation" ->   ObservationClass(INFTY, INFTY,INFTY,INFTY,INFTY,INFTY,INFTY,INFTY)
  )

  val LTBTS = WeakLTBTS ++ SpecialWeakEqs

  val LTBTSNotionNames = LTBTS.map(_._1).toSet

  def getSpectrumClass(name: String) = LTBTS.find(_._1 == name)

  /** given a group of least distinguishing observation classes, tell what weaker ObservationClasses would be the strongest fit to preorder the distinguished states */
  def getStrongestPreorderClass[A](leastClassifications: Iterable[(String, ObservationClass)]): List[(String, ObservationClass)] = {

    val weakerClasses = LTBTS.filterNot { c => leastClassifications.exists(c._2 above _._2) }
    val mostFitting = weakerClasses.filterNot { c => weakerClasses.exists(_._2 strictlyAbove c._2) }

    mostFitting.toList
  }
}
  