package de.bbisping.eqfiddle.spectroscopy

import de.bbisping.eqfiddle.ts.WeakTransitionSystem
import de.bbisping.eqfiddle.algo.AlgorithmLogging
import de.bbisping.eqfiddle.hml.ObservationClassFast
import de.bbisping.eqfiddle.game.SimpleGame
import de.bbisping.eqfiddle.game.SimpleGame.GameNode
import de.bbisping.eqfiddle.hml.HennessyMilnerLogic
import de.bbisping.eqfiddle.hml.HMLInterpreter

class FastSpectroscopy[S, A, L] (
    ts: WeakTransitionSystem[S, A, L],
    nodes: List[S])
  extends AlgorithmLogging[S] {

  val distinguishingFormulas = collection.mutable.Map[(GameNode, ObservationClassFast), Set[HennessyMilnerLogic.Formula[A]]]()

  def buildHMLWitness(game: FastSpectroscopyGame[S, A, L], node: GameNode, price: ObservationClassFast): Set[HennessyMilnerLogic.Formula[A]]
    = distinguishingFormulas.getOrElseUpdate((node, price), {
    //val currentVictoryPrices = game.attackerVictoryPrices(s)
    node match {
      case game.AttackerObservation(p0, qq0, false) if qq0.isEmpty =>
        Set(HennessyMilnerLogic.True)
      case game.AttackerObservation(p0, qq0, postConj0) =>
        val successorFormulas =
          for {
            s <- game.successors(node)
            sP <- game.attackerVictoryPrices(s)
            //currP <- currentVictoryPrices
            if sP strictlyBelow price // ensure descent
          } yield s match {
            case game.AttackerObservation(p1, qq1, postConj1) if !postConj0 && sP.observationHeight < price.observationHeight =>
              val possibleRestoredActions = for {
                (a, pp1) <- ts.post(p0)
                if pp1 contains p1
                qqq1 <- ts.post(qq0).get(a)
                if qq1 == qqq1
              } yield a
              val newPriceBound = ObservationClassFast(price.observationHeight - 1, price.conjunctionLevels, price.maxPositiveConjunctHeight, price.maxNegativeConjunctHeight)
              for {
                a <- possibleRestoredActions.headOption.toSet[A] // just take first option
                postForm <- buildHMLWitness(game, s, newPriceBound)
              } yield HennessyMilnerLogic.Observe(a, postForm)
            case game.AttackerObservation(p1, qq1, postConj1) if postConj0 =>
              {
                if (Set(p1) == qq0 && Set(p0) == qq1 && price.maxNegativeConjunctHeight >= sP.observationHeight) { // side swap
                  val newPrice = ObservationClassFast(Math.max(price.observationHeight, price.maxNegativeConjunctHeight),
                    price.conjunctionLevels, price.maxPositiveConjunctHeight, price.maxNegativeConjunctHeight)
                  for {
                    postForm <- buildHMLWitness(game, s, newPrice)
                  } yield HennessyMilnerLogic.Negate(postForm)
                } else {
                  Set[HennessyMilnerLogic.Formula[A]]()
                }
              } ++ {
                val newPrice = ObservationClassFast(Math.max(price.observationHeight, price.maxPositiveConjunctHeight),
                  price.conjunctionLevels, price.maxPositiveConjunctHeight, price.maxNegativeConjunctHeight)
                if (p0 == p1 && qq0 == qq1 && price.maxPositiveConjunctHeight >= sP.observationHeight) { // no side swap (= positive branch)
                  buildHMLWitness(game, s, price)
                } else {
                  Set[HennessyMilnerLogic.Formula[A]]()
                }
              }
            case game.DefenderConjunction(p1, qq1) =>
              val newPriceBound = ObservationClassFast(price.observationHeight, price.conjunctionLevels - 1, price.maxPositiveConjunctHeight, price.maxNegativeConjunctHeight)
              buildHMLWitness(game, s, newPriceBound)
              //List(ObservationClassFast(conjunctionLevels = sP.observationHeight) lub sP)
            case _ => Set()
          }
        successorFormulas.toSet.flatten
      case defNode: game.DefenderConjunction =>
        val possibleMoves = for {
          s <- game.successors(node)
          sP <- game.attackerVictoryPrices(s)
          if sP strictlyBelow price // ensure descent
        } yield buildHMLWitness(game, s, price)

        val productMoves =
          possibleMoves.foldLeft(Seq(Seq[HennessyMilnerLogic.Formula[A]]()))(
            (b, a) => b.flatMap(i => a.map(j => i ++ Seq(j))))
        productMoves.map { mv =>
          val moves = mv.toSet
          HennessyMilnerLogic.And(moves).asInstanceOf[HennessyMilnerLogic.Formula[A]]
        }.toSet
    }
  })

  def compute(): AbstractSpectroscopy.SpectroscopyResult[S,A] = {

    val hmlGame = new FastSpectroscopyGame(ts)

    val init = List(hmlGame.AttackerObservation(nodes(0), Set(nodes(1))), hmlGame.AttackerObservation(nodes(1), Set(nodes(0))))
    def instantAttackerWin(gn: GameNode) = gn match {case hmlGame.DefenderConjunction(_, qq) if qq.isEmpty => List(ObservationClassFast()); case _ => List()}

    hmlGame.populateGame(
      init,
      hmlGame.computeSuccessors(_),
      instantAttackerWin(_))

    debugLog("HML spectroscopy game size: " + hmlGame.discovered.size)

    debugLog("Best prices: " + hmlGame.attackerVictoryPrices)

    for {
      bestPrice <- hmlGame.attackerVictoryPrices(init.head)
      witnessFormula <- buildHMLWitness(hmlGame, init.head, bestPrice)
    } {
      debugLog("Distinguished under " + witnessFormula.classifyFormula() + " preorder by " + witnessFormula.toString())
      checkDistinguishing(witnessFormula, nodes(0), nodes(1))
    }

    AbstractSpectroscopy.SpectroscopyResult[S,A](List())
    // val attackerWin = hmlGame.computeWinningRegion()
    // val aLR = hmlGame.AttackerObservation(nodes(0), Set(nodes(1)), hmlGame.ConjunctMove)
    // val aRL = hmlGame.AttackerObservation(nodes(1), Set(nodes(0)), hmlGame.ConjunctMove)

    // val minFormulas = buildHML(hmlGame, attackerWin, Set(aLR, aRL))

    // if (attackerWin.contains(aLR)) {
    //   minFormulas(aLR).foreach { f =>
    //     debugLog("Distinguished under " + f.classifyFormula() + " preorder by " + f.toString())
    //     checkDistinguishing(f, nodes(0), nodes(1))
    //   }
    // }

    // if (attackerWin.contains(aRL)) {
    //   minFormulas(aRL).foreach { f =>
    //     debugLog("Distinguished under " + f.classifyFormula() + " preorder by " + f.toString())
    //     checkDistinguishing(f, nodes(1), nodes(0))
    //   }
    // }
    // debugLog(graphvizGameWithFormulas(hmlGame, attackerWin, minFormulas))

    //collectSpectroscopyResult(hmlGame, minFormulas)
  }


  def checkDistinguishing(formula: HennessyMilnerLogic.Formula[A], p: S, q: S) = {
    val hmlInterpreter = new HMLInterpreter(ts)
    val check = hmlInterpreter.isTrueAt(formula, List(p, q))
    if (!check(p) || check(q)) {
      System.err.println("Formula " + formula.toString() + " is no sound distinguishing formula! " + check)
    }
  }
}