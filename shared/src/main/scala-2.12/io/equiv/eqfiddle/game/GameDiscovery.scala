package io.equiv.eqfiddle.game

trait AbstractGameDiscovery {
  self: SimpleGame =>

  /** part of the game that can be reached from the initial nodes starting in the `initialNodes`. (warning: mutable!) */
  val discovered = collection.mutable.Set[GameNode]()

}

/**
 * this function can add a `predecessors`-function to a game that is only specified
 * in terms of some given nodes and the `successors`-function. it also adds a map for the
 * number of successors for discovered nodes `successorNum`.
 */
trait GameDiscovery extends AbstractGameDiscovery {
  self: SimpleGame =>
  
  /** which nodes to start the discovery in. */
  def initialNodes: Iterable[GameNode]

  /** number of successor states of discovered states (warning: mutable!) */
  val successorNum = collection.mutable.Map[GameNode, Int]() withDefaultValue 0
  
  override def predecessors(gn: GameNode): Iterable[GameNode] = computedPredecessors(gn)
  
  private val computedPredecessors = collection.mutable.Map[GameNode, Set[GameNode]]() withDefaultValue Set()
  
  // discover relevant game nodes and count outgoing transitions
  
  private val todo = new collection.mutable.Queue[GameNode]()
  todo ++= initialNodes
  discovered ++= initialNodes
  
  while (todo.nonEmpty) {
    val currNode = todo.dequeue()
    val succs = successors(currNode)
    successorNum(currNode) = succs.size
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
}