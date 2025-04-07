package io.equiv.eqfiddle.game

import io.equiv.eqfiddle.algo.AlgorithmLogging

trait GameLazyDecision[GamePosition <: SimpleGame.GamePosition, P] extends AbstractGameDiscovery[GamePosition] {
  self: SimpleGame[GamePosition] =>

  override def predecessors(gn: GamePosition): Iterable[GamePosition] = computedPredecessors(gn)
  private val computedPredecessors = collection.mutable.Map[GamePosition, Set[GamePosition]]() withDefaultValue Set()

  override def successors(gn: GamePosition): Iterable[GamePosition] = computedSuccessors(gn)
  private val computedSuccessors = collection.mutable.Map[GamePosition, Set[GamePosition]]() withDefaultValue Set()
  def computeSuccessors(gn: GamePosition): Iterable[GamePosition]

  /* set for nodes that are won by the attacker with the minimal attacker victory prices */
  val attackerWinningBudgets = collection.mutable.Map[GamePosition, List[P]]() withDefaultValue List()

  def isAttackerWinningEnergy(gn: GamePosition, p: P) = attackerWinningBudgets(gn).exists(mwp => energyIsLowerOrEq(mwp, p))

  /* price p1 is strictly better than p2 for an attacker win */
  def energyIsLower(p1: P, p2: P): Boolean

  /* price p1 is better than or equivalent to p2 for an attacker win */
  def energyIsLowerOrEq(p1: P, p2: P): Boolean

  def showSuccessors(): String = computedSuccessors.toString

  def gameSize(): (Int, Int) = (discovered.size, computedSuccessors.values.map(_.size).sum)

  /* output todo length (for debugging purposes) */
  val printToDoLength: Boolean = false

  private def energyUpdate(node: GamePosition, newBudgets: Iterable[P]) = {
    val oldBudgets = attackerWinningBudgets(node)
    val newBudgetsMin = newBudgets.filterNot(p => newBudgets.exists(op => energyIsLower(op, p))).toList
    if (newBudgetsMin.forall(p => oldBudgets.contains(p))) {
      if (!attackerWinningBudgets.isDefinedAt(node)) {
        // we should save the (presumably empty) list of budgets. (so later algorithms know that we looked for an attacker win here without success.)
        attackerWinningBudgets(node) = newBudgetsMin
      }
      false
    } else {
      attackerWinningBudgets(node) = newBudgetsMin
      true
    }
  }

  /** compute the current budget for a position (likely using information about successors.) */
  def computeCurrentBudget(node: GamePosition): Iterable[P]

  /** Fill game graph and compute attacker wins during discovery. */
  def populateGame(
      initialPositions: Iterable[GamePosition]) = {

    val todo = collection.mutable.Queue[GamePosition]()
    val visited = collection.mutable.Set[GamePosition]()
    todo ++= initialPositions
    discovered ++= initialPositions

    while (todo.nonEmpty) {
      if (printToDoLength) AlgorithmLogging.debugLog(todo.size.toString())
      val currPos = todo.dequeue()
      if (!(visited contains currPos)) {
        visited += currPos
        val succs = computeSuccessors(currPos)
        computedSuccessors(currPos) = succs.toSet
        for {
          gn <- succs
        } {
          computedPredecessors(gn) += currPos
          if (!(discovered contains gn)) {
            discovered += gn
            todo += gn
          }
        }
      }
      val updatedBudget = computeCurrentBudget(currPos)
      if (energyUpdate(currPos, updatedBudget) ) {
        // propagate a new win
        predecessors(currPos).foreach(_ +=: todo)
      }
    }
  }
}