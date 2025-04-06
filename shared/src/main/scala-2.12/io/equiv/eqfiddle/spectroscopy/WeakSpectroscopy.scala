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
    ts: WeakTransitionSystem[S, A, L])
  extends SpectroscopyInterface[S, A, L, HennessyMilnerLogic.Formula[A]] with AlgorithmLogging[S] {

  import WeakSpectroscopyGame._
  type GamePosition = WeakSpectroscopyGamePosition[S, A]

  val spectrum = ObservationNotionWeak.LTBTS

  val distinguishingFormulas =
    collection.mutable.Map[(GamePosition, Energy), Iterable[HennessyMilnerLogic.Formula[A]]]()

  var gameSize = (0, 0)

  def pruneDominated(oldFormulas: Set[HennessyMilnerLogic.Formula[A]]) = {
    spectrum.getLeastDistinguishing(oldFormulas)
  }

  def buildHMLWitness(game: WeakSpectroscopyGame[S, A, L], node: GamePosition, price: Energy): Iterable[HennessyMilnerLogic.Formula[A]]
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
        val gameBranching = game.asInstanceOf[WeakSpectroscopyGameBranching[S, A, L]]

        // at first we collect optional distinguishing formulas for each q in qq0 as in usual conjunctions
        val possibleMoves: Iterable[Iterable[HennessyMilnerLogic.Formula[A]]] = for {
          s1 <- gameBranching.successors(node)
          update1 = gameBranching.weight(node, s1)
          newBudget1 = update1.applyEnergyUpdate(price)
        } yield (if (gameBranching.isAttackerWinningEnergy(s1, newBudget1)) {
          for {
            s2 <- gameBranching.successors(s1)
            update2 = gameBranching.weight(s1, s2)
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

  private def energyToClass(e: Energy): ObservationNotionWeak = {
    ObservationNotionWeak(e(0), e(1), e(2), e(3), e(4), e(5), e(6), e(7), e(8))
  }

  private def classToEnergy(obsClass: ObservationNotionWeak): Energy = {
    val c = obsClass.toTuple
    Energy(Array(c._1, c._2, c._3, c._4, c._5, c._6, c._7, c._8, c._9))
  }

  def compute(
      comparedPairs: Iterable[(S,S)],
      config: SpectroscopyInterface.SpectroscopyConfig = SpectroscopyInterface.SpectroscopyConfig()
    ): SpectroscopyInterface.SpectroscopyResult[S, A, ObservationNotionWeak, HennessyMilnerLogic.Formula[A]] = {

    debugLog(s"Start spectroscopy on ${ts.nodes.size} node transition system with ${comparedPairs.size} compared pairs.")

    val hmlGame = if (config.useBranchingSpectroscopyGame) {
      new WeakSpectroscopyGameBranching(ts, config)
    } else {
      new WeakSpectroscopyGame(ts, config)
    }

    val init = for {
      (p, q) <- comparedPairs
      start <- List(AttackerObservation(p, Set(q)), AttackerObservation(q, Set(p)))
    } yield start

    val zeroEnergySet = Set(Energy.zeroEnergy(9))

    def instantAttackerWin(gn: GamePosition) = gn match {
      case DefenderConjunction(_, qq) if qq.isEmpty => zeroEnergySet
      case _ => Set.empty
    }

    debugLog("HML spectroscopy game construction ...")

    hmlGame.populateGame(
      init,
      instantAttackerWin(_))

    debugLog("HML spectroscopy game size: " + hmlGame.discovered.size)

    if (config.computeFormulas) {
      for {
        gn <- init
        AttackerObservation(p, qq) = gn
        bestPrice <- hmlGame.attackerWinningBudgets(gn)
        energyClass = energyToClass(bestPrice)
        witnessFormulas = buildHMLWitness(hmlGame, gn, bestPrice)
        f <- witnessFormulas.headOption
      } {
        val formulaPrice = spectrum.classifyFormula(f)._1
        if (! (formulaPrice <= energyClass) ) {
          AlgorithmLogging.debugLog(s"ERROR: Formula $f ${formulaPrice.toTuple} too expensive; not below ${energyClass.toTuple}.", logLevel = 4)
        } else {
          if (formulaPrice < energyClass) {
            AlgorithmLogging.debugLog(s"WARNING: Witness formula $f ${formulaPrice.toTuple} is strictly cheaper than determined minimal distinction class ${energyClass.toTuple}!", logLevel = 5)
          }
          debugLog("Distinguished at " + energyClass.toTuple + ", " + spectrum.classifyClass(energyClass) + " preorder by " + f.toString() + " Price: " + spectrum.classifyFormula(f))
          checkDistinguishing(f, p, qq.head)
        }
      }
      val distinguishingNodeFormulas = for {
        (node, pricedFormulas) <- distinguishingFormulas
          .toSet[((GamePosition, Energy), Iterable[HennessyMilnerLogic.Formula[A]])]
          .groupBy(kv => kv._1._1)
        formulas = for {
          (_, formulasForPrice) <- pricedFormulas
          f <- spectrum.getLeastDistinguishing(formulasForPrice)
        } yield f
      } yield (node, formulas)

      val bisimilarNodes = for {
        gn <- hmlGame.discovered
        if (gn match { case AttackerObservation(_, qq) => qq.size == 1; case _ => false }) &&
          (!hmlGame.attackerWinningBudgets.isDefinedAt(gn) || hmlGame.attackerWinningBudgets(gn).isEmpty)
      } yield (gn, Set[HennessyMilnerLogic.Formula[A]]())

      val distinguishingNodeFormulasExtended = distinguishingNodeFormulas ++ bisimilarNodes

      val gameString = debugLog(
        graphvizGameWithFormulas(hmlGame, hmlGame.attackerWinningBudgets.toMap, distinguishingNodeFormulasExtended),
        asLink = "https://edotor.net/?engine=dot#"//"https://dreampuf.github.io/GraphvizOnline/#"
      )

      val bestPreorders: Map[GamePosition,List[Spectrum.EquivalenceNotion[ObservationNotionWeak]]] =
        distinguishingNodeFormulasExtended.mapValues { ffs =>
        val classes = ffs.flatMap(spectrum.classifyFormula(_)._2)
        spectrum.getStrongestPreorderClass(classes)
      }

      val spectroResults = for {
        gn <- hmlGame.discovered
        if gn.isInstanceOf[AttackerObservation[S, A]]
        AttackerObservation(p, qq) = gn
        if qq.size == 1
        q <- qq
        preorders <- bestPreorders.get(gn)
        distinctionFormulas = distinguishingNodeFormulasExtended(gn)
        distinctions = for {
          f <- distinctionFormulas.toList
          (price, eqs) = spectrum.classifyFormula(f)
        } yield (f, price, eqs)
      } yield SpectroscopyInterface.SpectroscopyResultItem[S, A, ObservationNotionWeak, HennessyMilnerLogic.Formula[A]](p, q, distinctions, preorders)

      if (config.saveGameSize) gameSize = hmlGame.gameSize()

      SpectroscopyInterface.SpectroscopyResult[S, A, ObservationNotionWeak, HennessyMilnerLogic.Formula[A]](spectroResults.toList, spectrum, meta = Map("game" -> gameString))
    } else {
      for {
        gn <- init
        AttackerObservation(p, qq) = gn
      } {
        hmlGame.attackerWinningBudgets(gn)
      }

      // handle bisimilar nodes
      val bisimilarNodes = for {
        gn <- hmlGame.discovered
        if (gn match { case AttackerObservation(_, qq) => qq.size == 1; case _ => false }) &&
          (!hmlGame.attackerWinningBudgets.isDefinedAt(gn) || hmlGame.attackerWinningBudgets(gn).isEmpty)
      } {
        hmlGame.attackerWinningBudgets(gn) = List()
      }

      debugLog(graphvizGameWithFormulas(hmlGame, hmlGame.attackerWinningBudgets.toMap, Map()), asLink = "https://dreampuf.github.io/GraphvizOnline/#")

      val bestPreorders: Map[GamePosition,(Set[ObservationNotionWeak],List[Spectrum.EquivalenceNotion[ObservationNotionWeak]])] =
        hmlGame.attackerWinningBudgets.toMap.mapValues { energies =>
        val fcs = energies.toSet[Energy].map(energyToClass _)
        (fcs, spectrum.getStrongestPreorderClassFromClass(fcs))
      }

      val spectroResults = for {
        gn <- hmlGame.discovered
        if gn.isInstanceOf[AttackerObservation[S, A]]
        AttackerObservation(p, qq) = gn
        if qq.size == 1
        q <- qq
        (prices, preorders) <- bestPreorders.get(gn)
        distinctions = for {
          price <- prices
        } yield (HennessyMilnerLogic.True[A], price, spectrum.classifyClass(price))
      } yield SpectroscopyInterface.SpectroscopyResultItem[S, A, ObservationNotionWeak, HennessyMilnerLogic.Formula[A]](p, q, distinctions.toList, preorders)

      if (config.saveGameSize) gameSize = hmlGame.gameSize()

      SpectroscopyInterface.SpectroscopyResult[S, A, ObservationNotionWeak, HennessyMilnerLogic.Formula[A]](spectroResults.toList, spectrum)
    }

  }

  def checkIndividualPreorder(
      comparedPairs: Iterable[(S,S)],
      notion: String,
      config: SpectroscopyInterface.SpectroscopyConfig = SpectroscopyInterface.SpectroscopyConfig()
  ): SpectroscopyInterface.IndividualNotionResult[S] = {
    val hmlGame =
      if (config.useBranchingSpectroscopyGame) {
        new WeakSpectroscopyGameBranching(ts, config)
      } else {
        new WeakSpectroscopyGame(ts, config)
      }

    val init = for {
      (p, q) <- comparedPairs
      start <- List(AttackerObservation(p, Set(q)), AttackerObservation(q, Set(p)))
    } yield start

    val notionEnergy = classToEnergy(spectrum.getSpectrumClass(notion).obsClass)

    def energyUpdate(gn1: GamePosition, gn2: GamePosition, energy: Energy): Option[Energy] = {
      val update = hmlGame.weight(gn1, gn2)
      val newEnergy = update.applyEnergyUpdateInfinity(energy)
      if (gn1.isInstanceOf[SimpleGame.DefenderPosition] || newEnergy.isNonNegative())
        Some(newEnergy)
      else
        None
    }

    // whether to consider the baseSuccessor as a relevant node for the attacker
    def preferredNodes(currentBaseNode: GamePosition, currentEnergy: Energy, baseSuccessor: GamePosition) = currentBaseNode match {
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

    val reachabilityGame: MaterializedEnergyGame[GamePosition, Energy] = new MaterializedEnergyGame[GamePosition, Energy](
      hmlGame, init, notionEnergy, energyUpdate, preferredNodes)

    val attackerWins = reachabilityGame.computeWinningRegion()
    if (config.saveGameSize) gameSize = reachabilityGame.gameSize()

    val gameString = debugLog(
      graphvizMaterializedGame(reachabilityGame, attackerWins),
      asLink = "https://edotor.net/?engine=dot#"//"https://dreampuf.github.io/GraphvizOnline/#"
    )

    val relation: Set[(S, String, S)] = for {
      gn <- reachabilityGame.discovered.toSet
      if !attackerWins(gn)
      (p, eString, q) <- gn match {
        case MaterializedEnergyGame.MaterializedAttackerPosition(AttackerObservation(p, qq), energy)
            if qq.size == 1 && energy == notionEnergy =>
          Some((p, "", qq.head))
        case _ =>
          None
      }
    } yield (p, eString,  q)

    val items = for {
      (p, q) <- comparedPairs
    } yield {
      SpectroscopyInterface.IndividualNotionResultItem(p, q, relation.contains((p, "", q)))
    }
    SpectroscopyInterface.IndividualNotionResult(items, relation, meta = Map("game" -> gameString))
  }

  def checkDistinguishing(formula: HennessyMilnerLogic.Formula[A], p: S, q: S) = {
    val hmlInterpreter = new HMLInterpreter(ts)
    val check = hmlInterpreter.isTrueAt(formula, List(p, q))
    if (!check(p) || check(q)) {
      AlgorithmLogging.debugLog("Formula " + formula.toString() + " is no sound distinguishing formula! " + check, logLevel = 4)
    }
  }

  def gamePositionToString(
      game: WeakSpectroscopyGame[S, A, L],
      gn: GamePosition) = {
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

  def graphvizGameWithFormulas(
      spectroscopyGame: WeakSpectroscopyGame[S, A, L],
      attackerWinningBudgets: Map[GamePosition, Iterable[Energy]],
      formulas: Map[GamePosition, Set[HennessyMilnerLogic.Formula[A]]]
  ) = {
    val visualizer = new GameGraphVisualizer(spectroscopyGame) {
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

      def positionToID(gn: GamePosition): String =
        positionToType(gn) + gn.hashCode().toString().replace('-', 'n')

      def positionToString(gn: GamePosition): String = {
        val budgetString = attackerWinningBudgets.getOrElse(gn,Set()).map(_.vector.mkString("(",",",")")).mkString(" / ")
        val formulaString = formulas.getOrElse(gn,Set()).mkString("\\n").replaceAllLiterally("⟩⊤","⟩")
        gamePositionToString(spectroscopyGame, gn) +
         (if (budgetString != "") s"\\n------\\n$budgetString" else "") +
         (if (formulaString != "") s"\\n------\\n$formulaString" else "")
      }

      def moveToLabel(gn1: GamePosition, gn2: GamePosition) = spectroscopyGame.weight(gn1, gn2).toString()
    }

    val attackerWin = attackerWinningBudgets.filter(_._2.nonEmpty).keySet.toSet

    visualizer.outputDot(attackerWin)
  }

  import MaterializedEnergyGame._
  type MaterializedPosition = MaterializedGamePosition[GamePosition, Energy]

  def materializedToBaseGamePosition(gn: MaterializedPosition) = gn match {
    case MaterializedAttackerPosition(bgn, e) =>
      bgn
    case MaterializedDefenderPosition(bgn, e) =>
      bgn
  }

  def graphvizMaterializedGame(
      game: MaterializedEnergyGame[GamePosition, Energy],
      attackerWin: Set[MaterializedPosition]
  ) = {
    val baseGame = game.baseGame.asInstanceOf[WeakSpectroscopyGame[S, A, L]]
    val maxIntString = Int.MaxValue.toString()
    val visualizer = new GameGraphVisualizer(game) {

      def toEnergy(gn: MaterializedPosition) = gn match {
        case MaterializedAttackerPosition(bgn, e) =>
          e
        case MaterializedDefenderPosition(bgn, e) =>
          e
      }

      def positionToType(gn: MaterializedPosition): String = materializedToBaseGamePosition(gn) match {
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

      def positionToID(gn: MaterializedPosition): String =
        positionToType(gn) + gn.hashCode().toString().replace('-', 'n')

      def positionToString(gn: MaterializedPosition): String = {
        gamePositionToString(baseGame, materializedToBaseGamePosition(gn)) + "\\n" + toEnergy(gn).toString().replaceAllLiterally(maxIntString, "∞")
      }

      def moveToLabel(gn1: MaterializedPosition, gn2: MaterializedPosition) = {
        baseGame.weight(materializedToBaseGamePosition(gn1), materializedToBaseGamePosition(gn2)).toString()
      }

    }

    visualizer.outputDot(attackerWin)
  }
}