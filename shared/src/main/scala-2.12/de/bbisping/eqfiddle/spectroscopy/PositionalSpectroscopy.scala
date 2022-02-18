package de.bbisping.eqfiddle.spectroscopy

import de.bbisping.eqfiddle.util.Relation
import de.bbisping.eqfiddle.ts.WeakTransitionSystem
import de.bbisping.eqfiddle.util.FixedPoint
import de.bbisping.eqfiddle.game.WinningRegionComputation
import de.bbisping.eqfiddle.game.AttackGraphBuilder
import de.bbisping.eqfiddle.game.SimpleGame.GameNode
import de.bbisping.eqfiddle.algo.AlgorithmLogging
import de.bbisping.eqfiddle.util.LabeledRelation
import de.bbisping.eqfiddle.game.GameGraphVisualizer
import de.bbisping.eqfiddle.hml.HennessyMilnerLogic

class PositionalSpectroscopy[S, A, L] (
    ts: WeakTransitionSystem[S, A, L],
    nodes: List[S])
  extends AbstractSpectroscopy[S, A, L](ts, nodes) {

  override def pruneDominated(oldFormulas: Set[HennessyMilnerLogic.Formula[A]]) =
    HennessyMilnerLogic.getLeastDistinguishing(oldFormulas)

  def compute() = {

    val hmlGame = new SpectroscopyGame(ts, List((nodes(0), Set(nodes(1))), (nodes(1), Set(nodes(0)))))

    debugLog("HML spectroscopy game size: " + hmlGame.discovered.size)

    val attackerWin = hmlGame.computeWinningRegion()
    val aLR = hmlGame.AttackerObservation(nodes(0), Set(nodes(1)), hmlGame.ConjunctMove)
    val aRL = hmlGame.AttackerObservation(nodes(1), Set(nodes(0)), hmlGame.ConjunctMove)

    if (attackerWin.contains(aLR) || attackerWin.contains(aRL)) {
      val minFormulas = buildHML(hmlGame, attackerWin, Set(aLR, aRL))

      if (attackerWin.contains(aLR)) {
        minFormulas(aLR).foreach { f =>
          debugLog("Distinguished under " + f.classifyFormula() + " preorder by " + f.toString())
          checkDistinguishing(f, nodes(0), nodes(1))
        }
      }

      if (attackerWin.contains(aRL)) {
        minFormulas(aRL).foreach { f =>
          debugLog("Distinguished under " + f.classifyFormula() + " preorder by " + f.toString())
          checkDistinguishing(f, nodes(1), nodes(0))
        }
      }
      debugLog(graphvizGameWithFormulas(hmlGame, attackerWin, minFormulas))
    } else {

      val simNodes = for {
        gn <- hmlGame.discovered
        if gn.isInstanceOf[hmlGame.AttackerObservation] && !attackerWin(gn)
        hmlGame.AttackerObservation(p, qq, _) = gn
        q <- qq
      } yield (p, "", q)

      val rel = new LabeledRelation(simNodes.toSet)
      logRelation(rel, nodes(0) + " and " + nodes(1) + " are bisimulation equivalent.")

      debugLog(graphvizGameWithFormulas(hmlGame, attackerWin, Map().withDefaultValue(Set())))
    }

    true
  }
}