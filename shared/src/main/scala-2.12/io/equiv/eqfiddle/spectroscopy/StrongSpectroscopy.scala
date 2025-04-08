package io.equiv.eqfiddle.spectroscopy

import io.equiv.eqfiddle.ts.WeakTransitionSystem
import io.equiv.eqfiddle.algo.AlgorithmLogging
import io.equiv.eqfiddle.hml.StrongObservationNotion
import io.equiv.eqfiddle.hml.Spectrum
import io.equiv.eqfiddle.game.SimpleGame
import io.equiv.eqfiddle.game.EnergyGame
import io.equiv.eqfiddle.game.EnergyGame.Energy
import io.equiv.eqfiddle.game.MaterializedEnergyGame
import io.equiv.eqfiddle.hml.HML
import io.equiv.eqfiddle.hml.Interpreter
import io.equiv.eqfiddle.game.GameGraphVisualizer


class StrongSpectroscopy[S, A, L] (
    override val ts: WeakTransitionSystem[S, A, L])
  extends Spectroscopy[S, A, L, HML.Formula[A]]
  with SpectroscopyFramework[S, A, L, HML.Formula[A]] 
  with EquivalenceChecking[S, A, L, HML.Formula[A]] {

  import StrongSpectroscopyGame._

  type Notion = StrongObservationNotion
  type SpectroscopyGame = StrongSpectroscopyGame[S, A, L]
  type GamePosition = StrongSpectroscopyGamePosition[S]

  val spectrum = StrongObservationNotion.LTBTS

  override def notionToEnergy(obsNotion: Notion): Energy = {
    val c = obsNotion.toTuple
    Energy(Array(c._1, c._2, c._3, c._4, c._5, c._6))
  }
  override def energyToNotion(energy: Energy): Notion = {
    StrongObservationNotion(energy(0), energy(1), energy(2), energy(3), energy(4), energy(5))
  }

  override def openSpectroscopyGame(config: Spectroscopy.Config): SpectroscopyGame = {
    new StrongSpectroscopyGame[S, A, L](ts, config)
  }

  override def relationItemToGamePosition(p: S, q: S): GamePosition = 
    AttackerObservation(p, Set(q))

  override def gamePositionToRelationItem(gp: GamePosition): Option[(S, S)] = gp match {
    case AttackerObservation(p, qq) if qq.size == 1 =>
      Some((p, qq.head))
    case _ => None
  }

  def buildHMLWitness(game: SpectroscopyGame, node: GamePosition, price: Energy): Iterable[HML.Formula[A]]
    = distinguishingFormulas.getOrElseUpdate((node, price), {
    node match {
      case AttackerObservation(p0, qq0) if qq0.isEmpty =>
        Set(HML.True)
      case AttackerObservation(p0, qq0) =>
        val successorFormulas =
          for {
            s <- game.successors(node)
            update = game.weight(node, s)
            newBudget = update.applyEnergyUpdate(price)
            if game.isAttackerWinningEnergy(s, newBudget)
          } yield s match {
            case AttackerObservation(p1, qq1) =>
              val possibleRestoredActions = for {
                (a, pp1) <- ts.post(p0)
                if pp1 contains p1
                if qq1 == ts.post(qq0,a)
              } yield a
              for {
                a <- possibleRestoredActions.headOption.toList // just take first option
                postForm <- buildHMLWitness(game, s, newBudget)
              } yield HML.Observe(a, postForm)
            case DefenderConjunction(_, _, _) =>
              buildHMLWitness(game, s, newBudget)
            case _ => Set()
          }
        successorFormulas.flatten.toSet
      case AttackerConjunct(p0, q0) =>
        val successorFormulas = for {
          s <- game.successors(node)
          update = game.weight(node, s)
          newBudget = update.applyEnergyUpdate(price)
          if game.isAttackerWinningEnergy(s, newBudget)
        } yield {
          s match {
            case AttackerObservation(p1, qq1) =>
              if (p0 == p1) {
                buildHMLWitness(game, s, newBudget)
              } else {
                for {
                  postForm <- buildHMLWitness(game, s, newBudget)
                } yield HML.Negate(postForm)
              }
            }
          }
        successorFormulas.flatten
      case DefenderConjunction(_, _, _) =>
        val possibleMoves = for {
          s <- game.successors(node)
          update = game.weight(node, s)
          newBudget = update.applyEnergyUpdate(price)
        } yield if (game.isAttackerWinningEnergy(s, newBudget)) {
          buildHMLWitness(game, s, newBudget)
        } else {
          Set()
        }
        val productMoves =
          possibleMoves.foldLeft(Seq(Seq[HML.Formula[A]]()))(
            (b, a) => b.flatMap(i => a.map(j => i ++ Seq(j))))
        productMoves.map { mv =>
          val moves = mv.toSet
          HML.And(moves).asInstanceOf[HML.Formula[A]]
        }
    }
  })

  override def gamePositionToString(
      gn: GamePosition) = {
    val str = gn match {
      case AttackerObservation(p, qq: Set[_]) =>
        val qqString = qq.mkString("{",",","}")
        s"$p, $qqString"
      case AttackerConjunct(p, q) =>
        s"$p, $q"
      case DefenderConjunction(p, qqS: Set[_], qqR: Set[_]) =>
        val qqSString = qqS.mkString("{",",","}")
        val qqRString = qqR.mkString("{",",","}")
        s"$p, $qqSString, $qqRString"
      case _ => "ERROR"
    }
    str.replaceAllLiterally(".0", "").replaceAllLiterally("\\", "\\\\")
  }

  override def gamePositionToID(gn: GamePosition): String =
    gn.hashCode().toString().replace('-', 'n')

  // whether to consider the baseSuccessor as a relevant node for the attacker
  def preferredPositions(config: Spectroscopy.Config)(currentBaseNode: GamePosition, currentEnergy: Energy, baseSuccessor: GamePosition): Boolean = {
    (currentBaseNode match {
      case AttackerObservation(p, qq) if currentEnergy(1) >= Int.MaxValue && currentEnergy(2) >= Int.MaxValue && currentEnergy(3) >= Int.MaxValue && qq.size > 1 =>
        // if we have infinitely many conjunctions of unbounded positive depth, use them to chop down blowup on right-hand side
        baseSuccessor.isInstanceOf[DefenderConjunction[S]]
      case AttackerObservation(p, qq) if (currentEnergy(1) == 0) && qq.size >= 1 =>
        // dont use conjunction moves the attacker cannot survive
        !baseSuccessor.isInstanceOf[DefenderConjunction[S]]
      case _ => true
    }) && (
    // also: don't consider revivals if they make no difference!
      baseSuccessor match {
        case DefenderConjunction(p1, qq1, qqRevival) =>
          currentEnergy(2) != currentEnergy(3) || qqRevival.isEmpty
        case _ => true
    })
  }

}
