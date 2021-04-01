package de.bbisping.coupledsim.hml

case class ObservationClass(
  /** the maximal depth of the subformulas (⊤ has height 0, negation are neutral wrt. height) */
  height: Int = 0,
  /** the maximal amount of conjunctions when descending into a formula */
  conjunctionLevels: Int = 0,
  /** the maximal amount of negations when descending into a formula */
  negationLevels: Int = 0,
  /** the maximal amount of positive deep branches */
  maxPositiveDeepBranches: Int = 0,
  /** the maximal amount of positive flat branches (height > 1); if all branches are flat, one positive branch will be counted as deep */
  maxPositiveFlatBranches: Int = 0,
  /** maximal height of negative subformulas */
  maxNegationHeight: Int = 0,
  /** if there are any conjunctions with positive subformulas */
  nonNegativeConjuncts: Boolean = false,
  /** if there are conjunctions with positive AND negative parts */
  mixedConjuncts: Boolean = false,
  /** if there are conjunctions / negations that are not immediately preceeded by possible internal activity */
  immediateConj: Boolean = false,
  /** how many immediate observations may occur within weak conjunctions? */
  etaConjObs: Int = 0
) {
  def lub(that: ObservationClass) = ObservationClass(
    Integer.max(this.height, that.height),
    Integer.max(this.conjunctionLevels, that.conjunctionLevels),
    Integer.max(this.negationLevels, that.negationLevels),
    Integer.max(this.maxPositiveDeepBranches, that.maxPositiveDeepBranches),
    Integer.max(this.maxPositiveFlatBranches, that.maxPositiveFlatBranches),
    Integer.max(this.maxNegationHeight, that.maxNegationHeight),
    this.nonNegativeConjuncts || that.nonNegativeConjuncts,
    this.mixedConjuncts || that.mixedConjuncts,
    this.immediateConj || that.immediateConj,
    Integer.max(this.etaConjObs, that.etaConjObs)
  )

  def glb(that: ObservationClass) = ObservationClass(
    Integer.min(this.height, that.height),
    Integer.min(this.conjunctionLevels, that.conjunctionLevels),
    Integer.min(this.negationLevels, that.negationLevels),
    Integer.min(this.maxPositiveDeepBranches, that.maxPositiveDeepBranches),
    Integer.min(this.maxPositiveFlatBranches, that.maxPositiveFlatBranches),
    Integer.min(this.maxNegationHeight, that.maxNegationHeight),
    this.nonNegativeConjuncts && that.nonNegativeConjuncts,
    this.mixedConjuncts && that.mixedConjuncts,
    this.immediateConj && that.immediateConj,
    Integer.min(this.etaConjObs, that.etaConjObs)
  )

  def above(that: ObservationClass) = (
    this.height >= that.height &&
    this.conjunctionLevels >= that.conjunctionLevels &&
    this.negationLevels >= that.negationLevels &&
    this.maxPositiveDeepBranches >= that.maxPositiveDeepBranches &&
    this.maxPositiveFlatBranches >= that.maxPositiveFlatBranches &&
    this.maxNegationHeight >= that.maxNegationHeight &&
    (this.nonNegativeConjuncts || !that.nonNegativeConjuncts) &&
    (this.mixedConjuncts || !that.mixedConjuncts) &&
    (this.immediateConj || !that.immediateConj) &&
    this.etaConjObs >= that.etaConjObs
  )

  def strictlyAbove(that: ObservationClass) = (this != that) && (this above that)

  def below(that: ObservationClass) = (
    this.height <= that.height &&
    this.conjunctionLevels <= that.conjunctionLevels &&
    this.negationLevels <= that.negationLevels &&
    this.maxPositiveDeepBranches <= that.maxPositiveDeepBranches &&
    this.maxPositiveFlatBranches <= that.maxPositiveFlatBranches &&
    this.maxNegationHeight <= that.maxNegationHeight &&
    (!this.nonNegativeConjuncts || that.nonNegativeConjuncts) &&
    (!this.mixedConjuncts || that.mixedConjuncts) &&
    (!this.immediateConj || that.immediateConj) &&
    this.etaConjObs <= that.etaConjObs
  )

  def strictlyBelow(that: ObservationClass) = (this != that) && (this below that)
}

object ObservationClass {
  val INFTY = Integer.MAX_VALUE

  // height, conjunctionLevels, negationLevels, maxPosDeep, maxNegDeep, maxPosFlat, maxNegH, nonNegConjs, mixedConjs
  // for the weak spectrum: immediateConj, etaConjObs
  // nonNegConjs is necessary, because maxPosFlat will sometimes count one positive flat branch as deep to account for trace equivalences. 
  // The Linear-time Branching-time Spectrum
  val LTBTS = List(
    // Strong notions
    "traces" ->             ObservationClass(INFTY,     0,    0,    0,    0,    0,false,  false,  true,  INFTY),
    "failure" ->            ObservationClass(INFTY,     1,    1,    0,    0,    1,false,  false,  true,  INFTY),
    "readiness" ->          ObservationClass(INFTY,     1,    1,    0,INFTY,    1, true,   true,  true,  INFTY),
    "failure-trace" ->      ObservationClass(INFTY, INFTY,    1,    1,    0,    1, true,   true,  true,  INFTY),
    "ready-trace" ->        ObservationClass(INFTY, INFTY,    1,    1,INFTY,    1, true,   true,  true,  INFTY),
    "impossible-future" ->  ObservationClass(INFTY,     1,    1,    0,    0,INFTY,false,  false,  true,  INFTY),
    "possible-future" ->    ObservationClass(INFTY,     1,    1,INFTY,INFTY,INFTY, true,   true,  true,  INFTY),
    "simulation" ->         ObservationClass(INFTY, INFTY,    0,INFTY,INFTY,    0, true,  false,  true,  INFTY),
    "ready-simulation" ->   ObservationClass(INFTY, INFTY,    1,INFTY,INFTY,    1, true,   true,  true,  INFTY),
    "2-nested-simulation"-> ObservationClass(INFTY, INFTY,    1,INFTY,INFTY,INFTY, true,   true,  true,  INFTY),
    "bisimulation" ->       ObservationClass(INFTY, INFTY,INFTY,INFTY,INFTY,INFTY, true,   true,  true,  INFTY),

    // Weak notions
    "weak-traces" ->             ObservationClass(INFTY,     0,    0,    0,    0,    0,false,  false,false, 0),
    "weak-failure" ->            ObservationClass(INFTY,     1,    1,    0,    0,    1,false,  false,false, 0),
    "weak-readiness" ->          ObservationClass(INFTY,     1,    1,    0,INFTY,    1, true,   true,false, 0),
    "weak-failure-trace" ->      ObservationClass(INFTY, INFTY,    1,    1,    0,    1, true,   true,false, 0),
    "weak-ready-trace" ->        ObservationClass(INFTY, INFTY,    1,    1,INFTY,    1, true,   true,false, 0),
    "weak-impossible-future" ->  ObservationClass(INFTY,     1,    1,    0,    0,INFTY,false,  false,false, 0),
    "weak-possible-future" ->    ObservationClass(INFTY,     1,    1,INFTY,INFTY,INFTY, true,  false,false, 0),
    "contrasimulation" ->        ObservationClass(INFTY, INFTY,INFTY,    0,    0,INFTY,false,  false,false, 0),
    "weak-simulation" ->         ObservationClass(INFTY, INFTY,    0,INFTY,INFTY,    0, true,  false,false, 0),
    "weak-ready-simulation" ->   ObservationClass(INFTY, INFTY,    1,INFTY,INFTY,    1, true,  false,false, 0),
    "weak-2-nested-simulation"-> ObservationClass(INFTY, INFTY,    1,INFTY,INFTY,INFTY, true,  false,false, 0),
    "coupled-simulation" ->      ObservationClass(INFTY, INFTY,INFTY,INFTY,INFTY,INFTY, true,  false,false, 0), // TODO!!
    "weak-bisimulation" ->       ObservationClass(INFTY, INFTY,INFTY,INFTY,INFTY,INFTY, true,   true,false, 0), // TODO
    "delay-bisimulation" ->      ObservationClass(INFTY, INFTY,INFTY,INFTY,INFTY,INFTY, true,   true, true, 0),
    "eta-bisimulation" ->        ObservationClass(INFTY, INFTY,INFTY,INFTY,INFTY,INFTY, true,   true,false, 1),
    "branching-bisimulation" ->  ObservationClass(INFTY, INFTY,INFTY,INFTY,INFTY,INFTY, true,   true, true, 1)
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
  