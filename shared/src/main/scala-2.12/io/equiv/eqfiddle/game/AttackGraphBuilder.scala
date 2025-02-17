package io.equiv.eqfiddle.game

import io.equiv.eqfiddle.util.Relation
import io.equiv.eqfiddle.game.SimpleGame.GamePosition

class AttackGraphBuilder[L] {

  def buildAttackGraph(
      game: SimpleGame,
      win: Set[GamePosition],
      nodes: Iterable[GamePosition]):
    Relation[GamePosition] = {

    val visited = collection.mutable.Set[GamePosition]()
    val edges = collection.mutable.Queue[(GamePosition, GamePosition)]()

    def buildAttackTreeEdges(node: GamePosition): Unit = {
      
      if (!visited(node)) {
        visited += node

        for (
          t <- game.successors(node)
        ) {
          if (win(t) && t != node) {
            edges += ((node, t))
            buildAttackTreeEdges(t)
          }
        }
      }
    }

    nodes.foreach(buildAttackTreeEdges(_))

    new Relation(edges.toSet)
  }

  def accumulatePrices(
      graph: Relation[GamePosition],
      priceCons: (GamePosition, GamePosition, L) => L,
      pricePick: (GamePosition, Iterable[L]) => L,
      supPrice: L,
      nodes: Iterable[GamePosition]): Map[GamePosition, L] = {
    
    val prices = collection.mutable.Map[GamePosition, L]()
    val priceToDo = collection.mutable.ListBuffer[GamePosition]()
    priceToDo.appendAll(nodes)

    while (priceToDo.nonEmpty) {
      val n = priceToDo.remove(0)

      val oldPrice = prices.get(n)

      if (oldPrice.isEmpty) {
        prices(n) = supPrice
      }

      val followUps = graph.values(n).map(s => (s, prices.get(s)))
      val instableFollowUps = followUps.filter(_._2.isEmpty)
      if (instableFollowUps.isEmpty) {
        val succPrices = for {
          (s, Some(p)) <- followUps
        } yield priceCons(n, s, p)
        val newPrice = pricePick(n, succPrices)
        if (!(oldPrice contains newPrice)) {
          prices(n) = newPrice
          val predecessorUpdates = graph.valuesInverse(n).filterNot(priceToDo contains _) 
          priceToDo.appendAll(predecessorUpdates)
        }
      } else {
        priceToDo.prependAll(instableFollowUps.map(_._1))
      }
    }

    prices.toMap
  }


  def accumulateNodePrices(
      graph: Relation[GamePosition],
      pricePick: (GamePosition, Iterable[L]) => L,
      supPrice: L,
      nodes: Iterable[GamePosition]): Map[GamePosition, L] = {

    val prices = collection.mutable.Map[GamePosition, L]()
    val priceToDo = collection.mutable.ListBuffer[GamePosition]()
    priceToDo.appendAll(nodes)

    while (priceToDo.nonEmpty) {
      val n = priceToDo.remove(0)

      val oldPrice = prices.get(n)

      if (oldPrice.isEmpty) {
        prices(n) = supPrice
      }

      val followUps = graph.values(n).map(s => (s, prices.get(s)))
      val instableFollowUps = followUps.filter(_._2.isEmpty)
      if (instableFollowUps.isEmpty) {
        val succPrices = for {
          (s, Some(p)) <- followUps
        } yield p
        val newPrice = pricePick(n, succPrices)
        if (!(oldPrice contains newPrice)) {
          prices(n) = newPrice
          val predecessorUpdates = graph.valuesInverse(n).filterNot(priceToDo contains _) 
          priceToDo.appendAll(predecessorUpdates)
        }
      } else {
        priceToDo.prependAll(instableFollowUps.map(_._1))
      }
    }

    prices.toMap
  }
}