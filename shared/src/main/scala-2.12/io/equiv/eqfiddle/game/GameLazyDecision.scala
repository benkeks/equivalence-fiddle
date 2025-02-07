package io.equiv.eqfiddle.game

trait GameLazyDecision[P] extends AbstractGameDiscovery {
  self: SimpleGame =>

  /** part of the game that can be reached from the initial nodes starting in the `initialNodes`. (warning: mutable!) */
  override val discovered = collection.mutable.Set[GameNode]()

  override def predecessors(gn: GameNode): Iterable[GameNode] = computedPredecessors(gn)
  private val computedPredecessors = collection.mutable.Map[GameNode, Set[GameNode]]() withDefaultValue Set()

  override def successors(gn: GameNode): Iterable[GameNode] = computedSuccessors(gn)
  private val computedSuccessors = collection.mutable.Map[GameNode, Set[GameNode]]() withDefaultValue Set()
  def computeSuccessors(gn: GameNode): Iterable[GameNode]

  /* set for nodes that are won by the attacker with the minimal attacker victory prices */
  val attackerVictoryPrices = collection.mutable.Map[GameNode, List[P]]() withDefaultValue List()

  def isAttackerWinningPrice(gn: GameNode, p: P) = attackerVictoryPrices(gn).exists(mwp => priceIsBetterOrEq(mwp, p))

  /* price p1 is strictly better than p2 for an attacker win */
  def priceIsBetter(p1: P, p2: P): Boolean

  /* price p1 is better than or equivalent to p2 for an attacker win */
  def priceIsBetterOrEq(p1: P, p2: P): Boolean

  def showSuccessors(): String = computedSuccessors.toString

  def gameSize(): (Int, Int) = (discovered.size, computedSuccessors.values.map(_.size).sum)

  /* output todo length (for debugging purposes) */
  val printToDoLength: Boolean = false

  private def priceUpdate(node: GameNode, newPrices: Iterable[P]) = {
    val oldPrices = attackerVictoryPrices(node)
    val newPricesMin = newPrices.filterNot(p => newPrices.exists(op => priceIsBetter(op, p))).toList
    if (newPricesMin.forall(p => oldPrices.contains(p))) {
      false
    } else {
      attackerVictoryPrices(node) = newPricesMin
      true
    }
  }

  def computeCurrentPrice(node: GameNode): Iterable[P]

  def populateGame(
      initialNodes: Iterable[GameNode],
      instantAttackerWin: GameNode => Iterable[P]) = {

    val todo = collection.mutable.Queue[GameNode]()
    val visited = collection.mutable.Set[GameNode]()
    todo ++= initialNodes
    discovered ++= initialNodes

    while (todo.nonEmpty) {
      if (printToDoLength) println(todo.size)
      val currNode = todo.dequeue()
      val instaWin = instantAttackerWin(currNode)
      if (instaWin.nonEmpty) {
        if (priceUpdate(currNode, instaWin)) {
          // propagate a new win
          predecessors(currNode).foreach(_ +=: todo)
        }
      } else {
        if (!(visited contains currNode)) {
          visited += currNode
          val succs = computeSuccessors(currNode)
          computedSuccessors(currNode) = succs.toSet
          for {
            gn <- succs
          } {
            computedPredecessors(gn) += currNode
            if (!(discovered contains gn)) {
              discovered += gn
              todo += gn
            }
          }
        }
        val updatedPrice = computeCurrentPrice(currNode)
        if (priceUpdate(currNode, updatedPrice) ) {
          // propagate a new win
          predecessors(currNode).foreach(_ +=: todo)
        }
      }
    }
  }
}