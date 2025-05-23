package io.equiv.eqfiddle.game

trait AbstractGameDiscovery[GamePosition <: SimpleGame.GamePosition] {
  self: SimpleGame[GamePosition] =>

  /** part of the game that can be reached from the initial nodes starting in the `initialPositions`. (warning: mutable!) */
  val discovered = collection.mutable.Set[GamePosition]()

}

/**
 * this function can add a `predecessors`-function to a game that is only specified
 * in terms of some given nodes and the `successors`-function. it also adds a map for the
 * number of successors for discovered nodes `successorNum`.
 */
trait GameDiscovery[GamePosition <: SimpleGame.GamePosition] extends AbstractGameDiscovery[GamePosition] {
  self: SimpleGame[GamePosition] =>

  /** number of successor states of discovered states (warning: mutable!) */
  val successorNum = collection.mutable.Map[GamePosition, Int]() withDefaultValue 0
  
  override def predecessors(gn: GamePosition): Iterable[GamePosition] = computedPredecessors(gn)
  
  private val computedPredecessors = collection.mutable.Map[GamePosition, Set[GamePosition]]() withDefaultValue Set()

  def gameSize(): (Int, Int) = (discovered.size, computedPredecessors.values.map(_.size).sum)

  // discover relevant game nodes and count outgoing transitions
  def populate(initialPositions: Iterable[GamePosition]) = {
    val todo = new collection.mutable.Queue[GamePosition]()
    todo ++= initialPositions
    discovered ++= initialPositions
    
    while (todo.nonEmpty) {
      val currPos = todo.dequeue()
      val succs = successors(currPos)
      successorNum(currPos) = succs.size
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
  }

}