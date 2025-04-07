package io.equiv.eqfiddle.hml

import io.equiv.eqfiddle.hml.HennessyMilnerLogic._
import io.equiv.eqfiddle.ts.WeakTransitionSystem
import io.equiv.eqfiddle.game.SimpleGame
import io.equiv.eqfiddle.game.WinningRegionComputation
import io.equiv.eqfiddle.game.GameDiscovery

class HMLInterpreter[S, A, L] (
    val ts: WeakTransitionSystem[S, A, L]) {
  
  trait HMLGamePosition[S, A] extends SimpleGame.GamePosition
  case class HMLAttack(s: S, formula: Formula[A]) extends SimpleGame.AttackerPosition with HMLGamePosition[S, A]
  case class HMLDefense(s: S, formula: Formula[A]) extends SimpleGame.DefenderPosition with HMLGamePosition[S, A]

  type GamePosition = HMLGamePosition[S, A]

  class HMLFormulaGame(formula: Formula[A], states: Iterable[S])
    extends SimpleGame[GamePosition] with GameDiscovery[GamePosition] with WinningRegionComputation[GamePosition] {

    def makeNode(s: S, formula: Formula[A]) = formula match {
      case Observe(_, _) | ObserveInternal(_, _) | Negate(And(_)) | Pass(_) =>
        HMLDefense(s, formula)
      case And(_) | Negate(_) => 
        HMLAttack(s, formula)
    }

    def successors(gn: GamePosition): Iterable[GamePosition] = gn match {
      case HMLAttack(s, And(subterms)) =>
        for {
          f <- subterms
        } yield makeNode(s, f)
      case HMLAttack(s, Negate(Observe(action, andThen))) =>
        for {
          s1 <- ts.post(s, action)
        } yield makeNode(s1, Negate(andThen))
      case HMLAttack(s, Negate(ObserveInternal(andThen, opt))) =>
        for {
          s1 <- ts.silentSteps.values(s) ++ (if (opt) Some(s) else None)
        } yield makeNode(s1, Negate(andThen))
      case HMLAttack(s, Negate(Pass(andThen))) =>
        for {
          s1 <- ts.silentReachable(s)
        } yield makeNode(s1, Negate(andThen))
      case HMLAttack(s, Negate(Negate(andThen))) =>
        List(makeNode(s, andThen))
      case HMLDefense(s, Negate(And(subterms))) =>
        for {
          f <- subterms
        } yield makeNode(s, Negate(f))
      case HMLDefense(s, Observe(action, andThen)) =>
        for {
          s1 <- ts.post(s, action)
        } yield makeNode(s1, andThen)
      case HMLDefense(s, ObserveInternal(andThen, opt)) =>
        for {
          s1 <- ts.silentSteps.values(s) ++ (if (opt) Some(s) else None)
        } yield makeNode(s1, andThen)
      case HMLDefense(s, Pass(andThen)) =>
        for {
          s1 <- ts.silentReachable(s)
        } yield makeNode(s1, andThen)
    }

    populate(
      for { s <- states } yield makeNode(s, formula)
    )
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