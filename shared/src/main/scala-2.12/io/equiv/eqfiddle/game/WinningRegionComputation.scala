package io.equiv.eqfiddle.game

/** algorithm for the computation of winning regions based on the algorithm from Kreutzer's
 *  lecture notes for "Logic, Games, Automata" */
trait WinningRegionComputation {
  self: SimpleGame with GameDiscovery =>
  
  /** returns the winning region of the attacker as a set. 
   *  WARNING: this function is destructive on `successorNum` */
  def computeWinningRegion() = {
    // this set collects the nodes won by the attacker.
    // initially, we assume that the defender wins everywhere.
    val win = collection.mutable.Set[GameNode]()
    
    // this procedure is called every time we are sure that a specific
    // game node is won by the attacker.
    def propagateAttackerWin(gn: GameNode) {
      if (!win(gn)) {
        win += gn
        for (pred <- predecessors(gn)) {
          successorNum(pred) -= 1
          // if the attacker can decide in `pred` to move to `gn` or if we just
          // killed the last remaining potentially winning move for the defender in
          // `pred`, then clearly the attacker wins in `pred`.
          if (pred.isInstanceOf[AttackerNode] || successorNum(pred) == 0) {
            propagateAttackerWin(pred)
          }
        }
      }
    }
    
    for {
      gn <- discovered
      if successorNum(gn) == 0 && !gn.isInstanceOf[AttackerNode]
    } {
      propagateAttackerWin(gn)
    }
    
    win.toSet
  }
  
}
