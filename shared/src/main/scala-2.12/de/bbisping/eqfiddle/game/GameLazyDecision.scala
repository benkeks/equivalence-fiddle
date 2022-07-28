package de.bbisping.eqfiddle.game

trait GameLazyDecision[P] {
  self: SimpleGame =>

  /** part of the game that can be reached from the initial nodes starting in the `initialNodes`. (warning: mutable!) */
  val discovered = collection.mutable.Set[GameNode]()

  override def predecessors(gn: GameNode): Iterable[GameNode] = computedPredecessors(gn)
  private val computedPredecessors = collection.mutable.Map[GameNode, Set[GameNode]]() withDefaultValue Set()

  override def successors(gn: GameNode): Iterable[GameNode] = computedSuccessors(gn)
  private val computedSuccessors = collection.mutable.Map[GameNode, Set[GameNode]]() withDefaultValue Set()

  /* set for nodes that are won by the attacker with the minimal attacker victory prices */
  val attackerVictoryPrices = collection.mutable.Map[GameNode, Set[P]]() withDefaultValue Set()

  /* price p1 is strictly better than p2 for an attacker win */
  def priceIsBetter(p1: P, p2: P): Boolean

  private def priceUpdate(node: GameNode, newPrices: Iterable[P]) = {
    // assumes old and new price sets not to contain any dominated prices
    val oldPrices = attackerVictoryPrices(node)
    val interestingNewPrices = newPrices.filterNot(p => oldPrices.exists(op => priceIsBetter(op, p) || op == p))
    if (interestingNewPrices.nonEmpty) {
      val interestingOldPrices = oldPrices.filterNot(p => interestingNewPrices.exists(priceIsBetter(_, p)))
      attackerVictoryPrices(node) = interestingOldPrices ++ interestingNewPrices
      true
    } else {
      false
    }
  }

  def computeCurrentPrice(node: GameNode): Iterable[P]

  def populateGame(
      initialNodes: Iterable[GameNode],
      discoverSuccessors: GameNode => Iterable[GameNode],
      instantAttackerWin: GameNode => Iterable[P]) = {

    val todo = new collection.mutable.Queue[GameNode]()
    val visited = collection.mutable.Set[GameNode]()
    todo ++= initialNodes
    discovered ++= initialNodes

    while (todo.nonEmpty) {
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
          val succs = discoverSuccessors(currNode)
          computedSuccessors(currNode) ++= succs
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
          println(todo)
          predecessors(currNode).foreach(n =>
            //if (!(todo contains n))
            n +=: todo)
        }
      }
    }
  }
}