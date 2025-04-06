package io.equiv.eqfiddle.spectroscopy

import io.equiv.eqfiddle.ts.WeakTransitionSystem
import io.equiv.eqfiddle.algo.AlgorithmLogging
import io.equiv.eqfiddle.hml.ObservationNotionStrong
import io.equiv.eqfiddle.hml.Spectrum
import io.equiv.eqfiddle.game.SimpleGame
import io.equiv.eqfiddle.game.EnergyGame
import io.equiv.eqfiddle.game.EnergyGame.Energy
import io.equiv.eqfiddle.hml.HennessyMilnerLogic
import io.equiv.eqfiddle.hml.HMLInterpreter
import io.equiv.eqfiddle.game.GameGraphVisualizer
import io.equiv.eqfiddle.hml.ObservationNotionWeak
import io.equiv.eqfiddle.util.FixedPoint
import io.equiv.eqfiddle.game.MaterializedEnergyGame

class WeakSpectroscopy[S, A, L] (
    override val ts: WeakTransitionSystem[S, A, L])
  extends SpectroscopyInterface[S, A, L, HennessyMilnerLogic.Formula[A]] {

  import WeakSpectroscopyGame._
  import MaterializedEnergyGame._

  type Notion = ObservationNotionWeak
  type GamePosition = WeakSpectroscopyGamePosition[S, A]
  type SpectroscopyGame = WeakSpectroscopyGame[S, A, L]

  val spectrum = ObservationNotionWeak.LTBTS

  override def buildSpectroscopyGame(config: SpectroscopyInterface.SpectroscopyConfig): SpectroscopyGame = {
    if (config.useBranchingSpectroscopyGame) {
      new WeakSpectroscopyGameBranching(ts, config)
    } else {
      new WeakSpectroscopyGame(ts, config)
    }
  }
  
  override def relationItemToGamePosition(p: S, q: S): GamePosition = 
    AttackerObservation(p, Set(q))

  override def gamePositionToRelationItem(gp: GamePosition): Option[(S, S)] = gp match {
    case AttackerObservation(p, qq) if qq.size == 1 =>
      Some((p, qq.head))
    case _ => None
  }

  def pruneDominated(oldFormulas: Set[HennessyMilnerLogic.Formula[A]]) = {
    spectrum.getLeastDistinguishing(oldFormulas)
  }

  def buildHMLWitness(game: SpectroscopyGame, node: GamePosition, price: Energy): Iterable[HennessyMilnerLogic.Formula[A]]
    = distinguishingFormulas.getOrElseUpdate((node, price), {
    node match {
      case AttackerObservation(p0, qq0) =>
        val successorFormulas =
          for {
            s <- game.successors(node)
            update = game.weight(node, s)
            newBudget = update.applyEnergyUpdate(price)
            if game.isAttackerWinningEnergy(s, newBudget)
          } yield s match {
            case AttackerDelayedObservation(p1, qq1) =>
              for {
                postForm <- buildHMLWitness(game, s, newBudget)
              } yield HennessyMilnerLogic.Pass(postForm)
            case DefenderConjunction(p1, qq1) =>
              buildHMLWitness(game, s, newBudget)
            case _ => Set()
          }
        //pruneDominated(successorFormulas.flatten.toSet)
        successorFormulas.headOption.flatMap(_.headOption)
      case ado: AttackerDelayedObservation[S, A] =>
        val attackerDelayedComponent = FixedPoint[Set[AttackerDelayedObservation[S, A]]]({
          case delayedTodo =>
            val newTodo = for {
              n: AttackerDelayedObservation[S, A] <- delayedTodo
              s <- game.successors(n)
              if s.isInstanceOf[AttackerDelayedObservation[S, A]] && game.isAttackerWinningEnergy(s, price)
            } yield s.asInstanceOf[AttackerDelayedObservation[S, A]]
            delayedTodo ++ newTodo
        }, (todo1, todo2) => todo1.size == todo2.size) (Set(ado))
        val successorFormulas =
          for {
            n @ AttackerDelayedObservation(p0, qq0) <- attackerDelayedComponent
            s <- game.successors(n)
            update = game.weight(n, s)
            newBudget = update.applyEnergyUpdate(price)
            if game.isAttackerWinningEnergy(s, newBudget) && !s.isInstanceOf[AttackerDelayedObservation[S, A]]
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
              } yield HennessyMilnerLogic.Observe(a, postForm)
            case DefenderConjunction(p1, qq1) =>
              buildHMLWitness(game, s, newBudget)
            case DefenderStableConjunction(p1, qq1, qq1revivals) =>
              buildHMLWitness(game, s, newBudget)
            case DefenderBranchingConjunction(p01, a, p1, qq01, qq01a) =>
              buildHMLWitness(game, s, newBudget)
            case _ => Set()
          }
        pruneDominated(successorFormulas.flatMap(_.headOption).toSet).headOption
        //successorFormulas.headOption.flatMap(_.headOption)
      case AttackerBranchingObservation(p0, qq0) =>
        for {
          s <- game.successors(node)
          update = game.weight(node, s)
          newBudget = update.applyEnergyUpdate(price)
          if game.isAttackerWinningEnergy(s, newBudget)
          f <- buildHMLWitness(game, s, newBudget)
        } yield if (s.isInstanceOf[AttackerDelayedObservation[S, A]]) HennessyMilnerLogic.Pass(f) else f
      case _ : AttackerConjunct[S, A] | _ : AttackerConjunctStable[S, A] =>
        val (p0, q0) = node match {
          case AttackerConjunct(p0, q0) => (p0, q0)
          case AttackerConjunctStable(p0, q0) => (p0, q0)
        }
        val successorFormulas = for {
          s <- game.successors(node)
          update = game.weight(node, s)
          newBudget = update.applyEnergyUpdate(price)
          if game.isAttackerWinningEnergy(s, newBudget)
        } yield {
          s match {
            case AttackerDelayedObservation(p1, qq1) =>
              if (p0 == p1) {
                for {
                  postForm <- buildHMLWitness(game, s, newBudget)
                } yield HennessyMilnerLogic.Pass(postForm)
              } else {
                for {
                  postForm <- buildHMLWitness(game, s, newBudget)
                } yield HennessyMilnerLogic.Negate(HennessyMilnerLogic.Pass(postForm))
              }
            }
          }
        successorFormulas.flatten
      case DefenderBranchingConjunction(p0, a, p1, qq0, qq0a) if !game.config.useBranchingSpectroscopyGame =>
        val aBranches = for {
          s <- game.successors(node)
          if s.isInstanceOf[AttackerBranchingObservation[S, A]]
          update = game.weight(node, s)
          newBudget = update.applyEnergyUpdate(price)
          if game.isAttackerWinningEnergy(s, newBudget)
          subformula <- buildHMLWitness(game, s, newBudget)
        } yield {
          s match {
            case AttackerBranchingObservation(_, _) if ts.silentActions(a) =>
              HennessyMilnerLogic.ObserveInternal(subformula, opt = true)
            case AttackerBranchingObservation(_, _) =>
              HennessyMilnerLogic.Observe(a, subformula)
            case _ =>
              subformula
          }
        }
        val possibleMoves = (for {
          s <- game.successors(node)
          update = game.weight(node, s)
          newBudget = update.applyEnergyUpdate(price)
          if !s.isInstanceOf[AttackerBranchingObservation[S, A]]
        } yield if (game.isAttackerWinningEnergy(s, newBudget)) {
          (buildHMLWitness(game, s, newBudget))
        } else {
          Seq()
        }) ++ Seq(aBranches)
        val productMoves =
          possibleMoves.foldLeft(Seq(Seq[HennessyMilnerLogic.Formula[A]]()))(
            (b, a) => b.flatMap(i => a.map(j => i ++ Seq(j))))
        val conjs = productMoves.map { mv =>
          val moves = mv.toSet
          HennessyMilnerLogic.And(moves).asInstanceOf[HennessyMilnerLogic.Formula[A]]
        }
        pruneDominated(conjs.toSet)
      case DefenderBranchingConjunction(p0, a, p1, qq0, qq0a) if game.config.useBranchingSpectroscopyGame =>
        // note: qq0a is always empty for branching-style game
        
        // at first we collect optional distinguishing formulas for each q in qq0 as in usual conjunctions
        val possibleMoves: Iterable[Iterable[HennessyMilnerLogic.Formula[A]]] = for {
          s1 <- game.successors(node)
          update1 = game.weight(node, s1)
          newBudget1 = update1.applyEnergyUpdate(price)
        } yield (if (game.isAttackerWinningEnergy(s1, newBudget1)) {
          for {
            s2 <- game.successors(s1)
            update2 = game.weight(s1, s2)
            newBudget2 = update2.applyEnergyUpdate(newBudget1)
            subformula <- buildHMLWitness(game, s2, newBudget2)
          } yield {
            s2 match {
              case AttackerBranchingObservation(_, _) if ts.silentActions(a) =>
                HennessyMilnerLogic.ObserveInternal(subformula, opt = true)
              case AttackerBranchingObservation(_, _) =>
                HennessyMilnerLogic.Observe(a, subformula)
              case _ =>
              subformula
            }
          }
        } else {
          Seq()
        })
        val productMoves: Seq[Seq[HennessyMilnerLogic.Formula[A]]] =
          possibleMoves.foldLeft(Seq(Seq[HennessyMilnerLogic.Formula[A]]()))(
            (b, a) => b.flatMap(i => a.map(j => i ++ Seq(j))))

        // we now remix the formulas such that the different branching conjuncts are merged. (their continuations are combined by a conjunction under the observation)
        val flattenedProducts = for {
          prod <- productMoves
        } yield {
          // note that only the branching conjuncts can lead to observes not guarded by ⟨ϵ⟩
          val (obsParts, conjParts) =
            prod.partition(part => part.isInstanceOf[HennessyMilnerLogic.ObserveInternal[A]] ||  part.isInstanceOf[HennessyMilnerLogic.Observe[A]])
          // collect and flatten continuations
          val obsContinuations = (for (obs <- obsParts) yield {
            obs match {
              case HennessyMilnerLogic.ObserveInternal(HennessyMilnerLogic.And(subterms), opt) => subterms
              case HennessyMilnerLogic.ObserveInternal(andThen, opt) => List(andThen)
              case HennessyMilnerLogic.Observe(action, HennessyMilnerLogic.And(subterms)) => subterms
              case HennessyMilnerLogic.Observe(action, andThen) => List(andThen)
            }
          }).flatten
          val branchingContinuation = if (obsContinuations.forall(
            c => c.isInstanceOf[HennessyMilnerLogic.Pass[A]] || HennessyMilnerLogic.isTrueLiteral(c)
          )) {
            HennessyMilnerLogic.Pass(HennessyMilnerLogic.And(obsContinuations.toSet))
          } else {
            HennessyMilnerLogic.And(obsContinuations.toSet)
          }
          // reconstruct conjunction with merged continuations
          (if (ts.silentActions(a)) {
            HennessyMilnerLogic.ObserveInternal(branchingContinuation, opt = true)
          } else {
            HennessyMilnerLogic.Observe(a, branchingContinuation)
          }) +: conjParts
        }
        
        val conjs = flattenedProducts.map { mv =>
          val moves = mv.toSet
          HennessyMilnerLogic.And(moves).asInstanceOf[HennessyMilnerLogic.Formula[A]]
        }
        pruneDominated(conjs.toSet)
      case DefenderConjunction(_, _) | DefenderStableConjunction(_, _, _) =>
        val possibleMoves = for {
          s <- game.successors(node)
          update = game.weight(node, s)
          newBudget = update.applyEnergyUpdate(price)
          if (s match {
            //case AttackerObservation(_, qq) => qq.nonEmpty
            case DefenderConjunction(_, _) => false
            case _ => true
          })
        } yield if (game.isAttackerWinningEnergy(s, newBudget)) {
          buildHMLWitness(game, s, newBudget)
        } else {
          Set()
        }
        val productMoves =
          possibleMoves.foldLeft(Seq(Seq[HennessyMilnerLogic.Formula[A]]()))(
            (b, a) => b.flatMap(i => a.map(j => i ++ Seq(j))))
        val conjs = productMoves.map { mv =>
          val moves = if (node.isInstanceOf[DefenderStableConjunction[S, A]]) {
            (mv :+ HennessyMilnerLogic.Negate(HennessyMilnerLogic.ObserveInternal(HennessyMilnerLogic.True))).toSet
          } else {
            mv.toSet
          }
          HennessyMilnerLogic.And(moves).asInstanceOf[HennessyMilnerLogic.Formula[A]]
        }
        pruneDominated(conjs.toSet)
    }
  })

  override def energyToNotion(e: Energy): Notion = {
    ObservationNotionWeak(e(0), e(1), e(2), e(3), e(4), e(5), e(6), e(7), e(8))
  }

  override def notionToEnergy(obsNotion: Notion): Energy = {
    val c = obsNotion.toTuple
    Energy(Array(c._1, c._2, c._3, c._4, c._5, c._6, c._7, c._8, c._9))
  }
  
  override def preferredPositions(config: SpectroscopyInterface.SpectroscopyConfig)(currentBaseNode: GamePosition, currentEnergy: Energy, baseSuccessor: GamePosition) = currentBaseNode match {
    case AttackerObservation(p, qq) if currentEnergy(4) >= Int.MaxValue && qq.size > 1 =>
      // if we have infinitely many immediate conjunctions, use them to chop down blowup on right-hand side
      baseSuccessor.isInstanceOf[DefenderConjunction[S, A]]
    case AttackerBranchingObservation(p, qq) if config.useBranchingSpectroscopyGame && currentEnergy(4) >= Int.MaxValue && qq.size > 1 =>
      // same as previous case
      baseSuccessor.isInstanceOf[DefenderConjunction[S, A]]
    case AttackerDelayedObservation(_, _) if (currentEnergy(3) == 0 || currentEnergy(5) == currentEnergy(6)) && baseSuccessor.isInstanceOf[DefenderStableConjunction[S, A]] =>
      // disregard revival partitions if they make no difference
      baseSuccessor.asInstanceOf[DefenderStableConjunction[S, A]].qqRevival.isEmpty
    case AttackerDelayedObservation(_, qq) if currentEnergy(1) >= Int.MaxValue && qq.size > 1 =>
      // focus on branching observations if we have infinite supply of them
      !baseSuccessor.isInstanceOf[AttackerObservation[S, A]]
    case _ => true
  }

  override def gamePositionToString(gn: GamePosition) = {
    val str = gn match {
      case AttackerObservation(p, qq: Set[_]) =>
        val qqString = qq.mkString("{",",","}")
        s"$p, $qqString"
      case AttackerDelayedObservation(p, qq: Set[_]) =>
        val qqString = qq.mkString("{",",","}")
        s"$p, ≈$qqString"
      case AttackerBranchingObservation(p, qq: Set[_]) =>
        val qqString = qq.mkString("{",",","}")
        s"$p, b$qqString"
      case AttackerConjunct(p, q) =>
        s"$p, $q"
      case AttackerConjunctStable(p, q) =>
        s"$p, s$q"
      case DefenderConjunction(p, qq: Set[_]) =>
        val qqString = qq.mkString("{",",","}")
        s"$p, d$qqString"
      case DefenderStableConjunction(p, qq: Set[_], qqRevivals) =>
        val qqString = qq.mkString("{",",","}")
        val qqRevivalsString = qq.mkString("{",",","}")
        s"$p, s$qqString, $qqRevivalsString"
      case DefenderBranchingConjunction(p0, a, p1, qq0, qq0a) =>
        s"$p0 -${a}-> $p1, ${qq0.mkString("{",",","}")}, ${qq0a.mkString("{",",","}")}"
      case AttackerBranchingConjunction(p0, a, p1, q0) =>
        s"$p0 -${a}-> $p1, ${q0}"
      case _ =>
        "ERROR"
    }
    str.replaceAllLiterally(".0", "").replaceAllLiterally("\\", "\\\\")
  }

  def positionToType(gn: GamePosition): String = gn match {
    case AttackerObservation(_, _) => "attackerObservation"
    case AttackerDelayedObservation(_, _) => "attackerDelayedObservation"
    case AttackerBranchingObservation(_, _) => "attackerBranchingObservation"
    case AttackerConjunct(_, _) => "attackerConjunct"
    case AttackerConjunctStable(_, _) => "attackerConjunctStable"
    case DefenderConjunction(_, _) => "defenderConjunction"
    case DefenderStableConjunction(_, _, _) => "defenderStableConjunction"
    case DefenderBranchingConjunction(_, _, _, _, _) => "defenderBranchingConjunction"
    case _ => "unknown"
  }

  override def gamePositionToID(gn: GamePosition): String =
    positionToType(gn) + gn.hashCode().toString().replace('-', 'n')

}