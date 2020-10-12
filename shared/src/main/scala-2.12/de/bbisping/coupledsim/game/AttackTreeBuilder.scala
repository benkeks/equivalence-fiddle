package de.bbisping.coupledsim.game

import de.bbisping.coupledsim.util.Relation
import de.bbisping.coupledsim.game.SimpleGame.GameNode

class AttackTreeBuilder[L] {

  def buildAttackTree(
      game: SimpleGame,
      win: Set[GameNode],
      node: GameNode):
    Relation[GameNode] = {

    val beingVisited = collection.mutable.Set[GameNode]()
    val visited = collection.mutable.Set[GameNode]()
    val edges = collection.mutable.Queue[(GameNode, GameNode)]()

    def buildAttackTreeEdges(node: GameNode): Unit = {
      
      if (!visited(node)) {
        visited += node
        beingVisited += node

        for (
          t <- game.successors(node)
        ) {
          if (win(t) && !beingVisited(t)) {
            edges += ((node, t))
            buildAttackTreeEdges(t)
          }
        }

        beingVisited -= node
      }
    }

    buildAttackTreeEdges(node)

    new Relation(edges.toSet)
  }

  def accumulatePrices(
      tree: Relation[GameNode],
      priceCons: (GameNode, GameNode, L) => L,
      pricePick: (GameNode, Iterable[L]) => L,
      finishingPrice: L,
      supPrice: L,
      node: GameNode,
      targetRegion: Set[GameNode]): Map[GameNode, L] = {
    
    val prices = collection.mutable.Map[GameNode, L]()
    
    def updatePrice(n: GameNode): Unit = {

      if (!prices.isDefinedAt(n)) {
        prices(n) = supPrice
        val succPrices = for {
          s <- tree.values(n)
        } yield {
          updatePrice(s)
          priceCons(n, s, prices(s))
        }
        prices(n) = pricePick(n, succPrices)
      }
    }

    updatePrice(node)

    prices.toMap
  }
}