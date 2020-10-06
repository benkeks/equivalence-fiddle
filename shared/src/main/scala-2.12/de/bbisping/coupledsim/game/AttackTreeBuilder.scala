package de.bbisping.coupledsim.game

import de.bbisping.coupledsim.util.Relation
import de.bbisping.coupledsim.game.SimpleGame.GameNode

class AttackTreeBuilder[L] {

  def buildAttackTree(
      game: SimpleGame,
      win: Set[GameNode],
      node: GameNode):
    Relation[GameNode] = {

    val visited = collection.mutable.Set[GameNode]()
    val edges = collection.mutable.Queue[(GameNode, GameNode)]()

    def buildAttackTreeEdges(
      game: SimpleGame,
      win: Set[GameNode],
      node: GameNode): Unit = {
      
      if (!visited(node)) {
        visited += node

        val winningMoves = game.successors(node).filter(n => win(n))

        val directEdges = for {
          t <- winningMoves
          if node != t
        } yield (node, t)

        for {
          t <- winningMoves
        } {
          buildAttackTreeEdges(game, win, t)
        } 

        edges ++= directEdges
      }
    }

    buildAttackTreeEdges(game, win, node)

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
    
    prices ++= targetRegion map ((_, finishingPrice))

    def updatePrice(n: GameNode): Unit = {
      if (!prices.isDefinedAt(n)) {
        prices(n) = supPrice
        val succPrices = for {
          s <- tree.rep.getOrElse(n, Set())
        } yield {
          updatePrice(s)
          priceCons(n, s, prices(s))
        }
        if (succPrices.nonEmpty) {
          prices(n) = pricePick(n, succPrices)
        }
      }
    }

    updatePrice(node)

    prices.toMap
  }
}