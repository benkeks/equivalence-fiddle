package de.bbisping.eqfiddle.hml

import de.bbisping.eqfiddle.hml.HennessyMilnerLogic._
import de.bbisping.eqfiddle.ts.WeakTransitionSystem
import de.bbisping.eqfiddle.game.SimpleGame
import de.bbisping.eqfiddle.game.WinningRegionComputation
import de.bbisping.eqfiddle.game.GameDiscovery

class HMLInterpreter[S, A, L] (
    val ts: WeakTransitionSystem[S, A, L]) {
  
  case class HMLAttack(s: S, formula: Formula[A]) extends SimpleGame.AttackerNode
  case class HMLDefense(s: S, formula: Formula[A]) extends SimpleGame.DefenderNode

  class HMLFormulaGame(formula: Formula[A], states: Iterable[S])
    extends SimpleGame with GameDiscovery with WinningRegionComputation {

    override def initialNodes: Iterable[GameNode] =
      for { s <- states } yield makeNode(s, formula)

    def makeNode(s: S, formula: Formula[A]) = formula match {
      case Observe(_, _) | Negate(And(_)) =>
        HMLDefense(s, formula)
      case And(_) | Negate(_)=> 
        HMLAttack(s, formula)
    }

    def successors(gn: GameNode): Iterable[GameNode] = gn match {
      case HMLAttack(s, And(subterms)) =>
        for {
          f <- subterms
        } yield makeNode(s, f)
      case HMLAttack(s, Negate(Observe(action, andThen))) =>
        for {
          s1 <- ts.weakPostDelay(s, action)
        } yield makeNode(s1, Negate(andThen))
      case HMLAttack(s, Negate(Negate(andThen))) =>
        List(makeNode(s, andThen))
      case HMLDefense(s, Negate(And(subterms))) =>
        for {
          f <- subterms
        } yield makeNode(s, Negate(f))
      case HMLDefense(s, Observe(action, andThen)) =>
        for {
          s1 <- ts.weakPostDelay(s, action)
        } yield makeNode(s1, andThen)
    }
  }

  def isTrueAt(f: Formula[A], states: Iterable[S]) = {
    val interpretationGame = new HMLFormulaGame(f, states)
    val attackerWin = interpretationGame.computeWinningRegion()
    val result = for {
      s <- states
    } yield (s, !attackerWin.contains(interpretationGame.makeNode(s, f)))
    result.toMap
  }

}