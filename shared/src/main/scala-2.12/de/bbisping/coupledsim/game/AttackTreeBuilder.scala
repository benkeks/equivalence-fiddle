package de.bbisping.coupledsim.game

import de.bbisping.coupledsim.util.LabeledRelation
import de.bbisping.coupledsim.game.SimpleGame.GameNode

class AttackTreeBuilder[L](weighting: (GameNode, GameNode) => L) {

  def buildAttackTree(
      game: SimpleGame,
      win: Set[GameNode],
      node: GameNode):
    LabeledRelation[GameNode, L] = {

    val visited = collection.mutable.Set[GameNode]()
    val edges = collection.mutable.Queue[(GameNode, L, GameNode)]()

    def buildAttackTreeEdges(
      game: SimpleGame,
      win: Set[GameNode],
      node: GameNode): Unit = {
      
      if (!visited(node)) {
        visited += node

        val winningMoves = game.successors(node).filter(win)

        val directEdges = for {
          t <- winningMoves
          if node != t
        } yield (node, weighting(node, t), t)

        for {
          t <- winningMoves
        } {
          buildAttackTreeEdges(game, win, t)
        } 

        edges ++= directEdges
      }
    }

    buildAttackTreeEdges(game, win, node)

    new LabeledRelation(edges.toSet)
  }

  def accumulatePrices(
      tree: LabeledRelation[GameNode, L],
      priceAdd: (L, L) => L,
      pricePick: Iterable[L] => L,
      initialPrice: L,
      supPrice: L,
      node: GameNode,
      targetRegion: Set[GameNode]): Map[GameNode, L] = {
    
    val prices = collection.mutable.Map[GameNode, L]()
    
    prices ++= targetRegion map ((_, initialPrice))

    def updatePrice(n: GameNode): Unit = {
      if (!prices.isDefinedAt(n)) {
        prices(n) = supPrice
        val succPrices = for {
          (l, ss) <- tree.rep.getOrElse(n, Map())
          s <- ss
        } yield {
          updatePrice(s)
          priceAdd(l, prices(s))
        }
        prices(n) = pricePick(succPrices)
      }
    }

    updatePrice(node)

    prices.toMap
  }
}