package io.equiv.eqfiddle.spectroscopy

import io.equiv.eqfiddle.ts.WeakTransitionSystem
import io.equiv.eqfiddle.algo.AlgorithmLogging
import io.equiv.eqfiddle.hml.ObservationNotionStrong
import io.equiv.eqfiddle.hml.Spectrum
import io.equiv.eqfiddle.game.SimpleGame
import io.equiv.eqfiddle.game.SimpleGame.GamePosition
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

  val useCleverBranching: Boolean = true

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
      case game.AttackerObservation(p0, qq0) =>
        val successorFormulas =
          for {
            s <- game.successors(node)
            update = game.weight(node, s)
            newPrice = update.applyEnergyUpdate(price)
            if game.isAttackerWinningPrice(s, newPrice)
          } yield s match {
            case game.AttackerDelayedObservation(p1, qq1) =>
              for {
                postForm <- buildHMLWitness(game, s, newPrice)
              } yield HennessyMilnerLogic.Pass(postForm)
            case game.DefenderConjunction(p1, qq1) =>
              buildHMLWitness(game, s, newPrice)
            case _ => Set()
          }
        //pruneDominated(successorFormulas.flatten.toSet)
        successorFormulas.headOption.flatMap(_.headOption)
      case ado: game.AttackerDelayedObservation =>
        val attackerDelayedComponent = FixedPoint[Set[game.AttackerDelayedObservation]]({
          case delayedTodo =>
            val newTodo = for {
              n: game.AttackerDelayedObservation <- delayedTodo
              s <- game.successors(n)
              if s.isInstanceOf[game.AttackerDelayedObservation] && game.isAttackerWinningPrice(s, price)
            } yield s.asInstanceOf[game.AttackerDelayedObservation]
            delayedTodo ++ newTodo
        }, (todo1, todo2) => todo1.size == todo2.size) (Set(ado))
        val successorFormulas =
          for {
            n @ game.AttackerDelayedObservation(p0, qq0) <- attackerDelayedComponent
            s <- game.successors(n)
            update = game.weight(n, s)
            newPrice = update.applyEnergyUpdate(price)
            if game.isAttackerWinningPrice(s, newPrice) && !s.isInstanceOf[game.AttackerDelayedObservation]
          } yield s match {
            case game.AttackerObservation(p1, qq1) =>
              val possibleRestoredActions = for {
                (a, pp1) <- ts.post(p0)
                if pp1 contains p1
                if qq1 == ts.post(qq0,a)
              } yield a
              for {
                a <- possibleRestoredActions.headOption.toList // just take first option
                postForm <- buildHMLWitness(game, s, newPrice)
              } yield HennessyMilnerLogic.Observe(a, postForm)
            case game.DefenderConjunction(p1, qq1) =>
              buildHMLWitness(game, s, newPrice)
            case game.DefenderStableConjunction(p1, qq1, qq1revivals) =>
              buildHMLWitness(game, s, newPrice)
            case game.DefenderBranchingConjunction(p01, a, p1, qq01, qq01a) =>
              buildHMLWitness(game, s, newPrice)
            case _ => Set()
          }
        pruneDominated(successorFormulas.flatMap(_.headOption).toSet).headOption
        //successorFormulas.headOption.flatMap(_.headOption)
      case game.AttackerBranchingObservation(p0, qq0) =>
        for {
          s <- game.successors(node)
          update = game.weight(node, s)
          newPrice = update.applyEnergyUpdate(price)
          if game.isAttackerWinningPrice(s, newPrice)
          f <- buildHMLWitness(game, s, newPrice)
        } yield if (s.isInstanceOf[game.AttackerDelayedObservation]) HennessyMilnerLogic.Pass(f) else f
      case _ : game.AttackerClause | _ : game.AttackerClauseStable =>
        val (p0, q0) = node match {
          case game.AttackerClause(p0, q0) => (p0, q0)
          case game.AttackerClauseStable(p0, q0) => (p0, q0)
        }
        val successorFormulas = for {
          s <- game.successors(node)
          update = game.weight(node, s)
          newPrice = update.applyEnergyUpdate(price)
          if game.isAttackerWinningPrice(s, newPrice)
        } yield {
          s match {
            case game.AttackerDelayedObservation(p1, qq1) =>
              if (p0 == p1) {
                for {
                  postForm <- buildHMLWitness(game, s, newPrice)
                } yield HennessyMilnerLogic.Pass(postForm)
              } else {
                for {
                  postForm <- buildHMLWitness(game, s, newPrice)
                } yield HennessyMilnerLogic.Negate(HennessyMilnerLogic.Pass(postForm))
              }
            }
          }
        successorFormulas.flatten
      case game.DefenderBranchingConjunction(p0, a, p1, qq0, qq0a) if !useCleverBranching =>
        val aBranches = for {
          s <- game.successors(node)
          if s.isInstanceOf[game.AttackerBranchingObservation]
          update = game.weight(node, s)
          newPrice = update.applyEnergyUpdate(price)
          if game.isAttackerWinningPrice(s, newPrice)
          subformula <- buildHMLWitness(game, s, newPrice)
        } yield {
          s match {
            case game.AttackerBranchingObservation(_, _) if ts.silentActions(a) =>
              HennessyMilnerLogic.ObserveInternal(subformula, opt = true)
            case game.AttackerBranchingObservation(_, _) =>
              HennessyMilnerLogic.Observe(a, subformula)
            case _ =>
              subformula
          }
        }
        val possibleMoves = (for {
          s <- game.successors(node)
          update = game.weight(node, s)
          newPrice = update.applyEnergyUpdate(price)
          if !s.isInstanceOf[game.AttackerBranchingObservation]
        } yield if (game.isAttackerWinningPrice(s, newPrice)) {
          (buildHMLWitness(game, s, newPrice))
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
      case game.DefenderBranchingConjunction(p0, a, p1, qq0, qq0a) if useCleverBranching =>
        // note: qq0a is always empty for clever game
        val gameClever = game.asInstanceOf[WeakSpectroscopyGameClever[S, A, L]]

        // at first we collect optional distinguishing formulas for each q in qq0 as in usual conjunctions
        val possibleMoves: Iterable[Iterable[HennessyMilnerLogic.Formula[A]]] = for {
          s1 <- gameClever.successors(node)
          update1 = gameClever.weight(node, s1)
          newPrice1 = update1.applyEnergyUpdate(price)
        } yield (if (gameClever.isAttackerWinningPrice(s1, newPrice1)) {
          for {
            s2 <- gameClever.successors(s1)
            update2 = gameClever.weight(s1, s2)
            newPrice2 = update2.applyEnergyUpdate(newPrice1)
            subformula <- buildHMLWitness(game, s2, newPrice2)
          } yield {
            s2 match {
              case gameClever.AttackerBranchingObservation(_, _) if ts.silentActions(a) =>
                HennessyMilnerLogic.ObserveInternal(subformula, opt = true)
              case gameClever.AttackerBranchingObservation(_, _) =>
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
      case game.DefenderConjunction(_, _) | game.DefenderStableConjunction(_, _, _) =>
        val possibleMoves = for {
          s <- game.successors(node)
          update = game.weight(node, s)
          newPrice = update.applyEnergyUpdate(price)
          if (s match {
            //case game.AttackerObservation(_, qq) => qq.nonEmpty
            case game.DefenderConjunction(_, _) => false
            case _ => true
          })
        } yield if (game.isAttackerWinningPrice(s, newPrice)) {
          buildHMLWitness(game, s, newPrice)
        } else {
          Set()
        }
        val productMoves =
          possibleMoves.foldLeft(Seq(Seq[HennessyMilnerLogic.Formula[A]]()))(
            (b, a) => b.flatMap(i => a.map(j => i ++ Seq(j))))
        val conjs = productMoves.map { mv =>
          val moves = if (node.isInstanceOf[game.DefenderStableConjunction]) {
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
      comparedPairs: Iterable[(S,S)]
    ): SpectroscopyInterface.SpectroscopyResult[S, A, ObservationNotionWeak, HennessyMilnerLogic.Formula[A]] = {
    compute(comparedPairs, computeFormulas = true)
  }

  def compute(
      comparedPairs: Iterable[(S,S)],
      computeFormulas: Boolean = true,
      saveGameSize: Boolean = false
    ): SpectroscopyInterface.SpectroscopyResult[S, A, ObservationNotionWeak, HennessyMilnerLogic.Formula[A]] = {

    debugLog(s"Start spectroscopy on ${ts.nodes.size} node transition system with ${comparedPairs.size} compared pairs.")

    val hmlGame = if (useCleverBranching) {
      new WeakSpectroscopyGameClever(ts, energyCap = if (computeFormulas) Int.MaxValue else 3)
    } else {
      new WeakSpectroscopyGame(ts, energyCap = if (computeFormulas) Int.MaxValue else 3)
    }

    val init = for {
      (p, q) <- comparedPairs
      start <- List(hmlGame.AttackerObservation(p, Set(q)), hmlGame.AttackerObservation(q, Set(p)))
    } yield start

    val zeroEnergySet = Set(Energy.zeroEnergy(9))

    def instantAttackerWin(gn: GamePosition) = gn match {
      case hmlGame.DefenderConjunction(_, qq) if qq.isEmpty => zeroEnergySet
      case _ => Set.empty
    }

    debugLog("HML spectroscopy game construction ...")

    hmlGame.populateGame(
      init,
      instantAttackerWin(_))

    debugLog("HML spectroscopy game size: " + hmlGame.discovered.size)

    if (computeFormulas) {
      for {
        gn <- init
        hmlGame.AttackerObservation(p, qq) = gn
        bestPrice <- hmlGame.attackerVictoryPrices(gn)
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
        if (gn match { case hmlGame.AttackerObservation(_, qq) => qq.size == 1; case _ => false }) &&
          (!hmlGame.attackerVictoryPrices.isDefinedAt(gn) || hmlGame.attackerVictoryPrices(gn).isEmpty)
      } yield (gn, Set[HennessyMilnerLogic.Formula[A]]())

      val distinguishingNodeFormulasExtended = distinguishingNodeFormulas ++ bisimilarNodes

      val gameString = debugLog(
        graphvizGameWithFormulas(hmlGame, hmlGame.attackerVictoryPrices.toMap, distinguishingNodeFormulasExtended),
        asLink = "https://edotor.net/?engine=dot#"//"https://dreampuf.github.io/GraphvizOnline/#"
      )

      val bestPreorders: Map[GamePosition,List[Spectrum.EquivalenceNotion[ObservationNotionWeak]]] =
        distinguishingNodeFormulasExtended.mapValues { ffs =>
        val classes = ffs.flatMap(spectrum.classifyFormula(_)._2)
        spectrum.getStrongestPreorderClass(classes)
      }

      val spectroResults = for {
        gn <- hmlGame.discovered
        if gn.isInstanceOf[hmlGame.AttackerObservation]
        hmlGame.AttackerObservation(p, qq) = gn
        if qq.size == 1
        q <- qq
        preorders <- bestPreorders.get(gn)
        distinctionFormulas = distinguishingNodeFormulasExtended(gn)
        distinctions = for {
          f <- distinctionFormulas.toList
          (price, eqs) = spectrum.classifyFormula(f)
        } yield (f, price, eqs)
      } yield SpectroscopyInterface.SpectroscopyResultItem[S, A, ObservationNotionWeak, HennessyMilnerLogic.Formula[A]](p, q, distinctions, preorders)

      if (saveGameSize) gameSize = hmlGame.gameSize()

      SpectroscopyInterface.SpectroscopyResult[S, A, ObservationNotionWeak, HennessyMilnerLogic.Formula[A]](spectroResults.toList, spectrum, meta = Map("game" -> gameString))
    } else {
      for {
        gn <- init
        hmlGame.AttackerObservation(p, qq) = gn
      } {
        hmlGame.attackerVictoryPrices(gn)
      }

      // handle bisimilar nodes
      val bisimilarNodes = for {
        gn <- hmlGame.discovered
        if (gn match { case hmlGame.AttackerObservation(_, qq) => qq.size == 1; case _ => false }) &&
          (!hmlGame.attackerVictoryPrices.isDefinedAt(gn) || hmlGame.attackerVictoryPrices(gn).isEmpty)
      } {
        hmlGame.attackerVictoryPrices(gn) = List()
      }

      debugLog(graphvizGameWithFormulas(hmlGame, hmlGame.attackerVictoryPrices.toMap, Map()), asLink = "https://dreampuf.github.io/GraphvizOnline/#")

      val bestPreorders: Map[GamePosition,(Set[ObservationNotionWeak],List[Spectrum.EquivalenceNotion[ObservationNotionWeak]])] =
        hmlGame.attackerVictoryPrices.toMap.mapValues { energies =>
        val fcs = energies.toSet[Energy].map(energyToClass _)
        (fcs, spectrum.getStrongestPreorderClassFromClass(fcs))
      }

      val spectroResults = for {
        gn <- hmlGame.discovered
        if gn.isInstanceOf[hmlGame.AttackerObservation]
        hmlGame.AttackerObservation(p, qq) = gn
        if qq.size == 1
        q <- qq
        (prices, preorders) <- bestPreorders.get(gn)
        distinctions = for {
          price <- prices
        } yield (HennessyMilnerLogic.True[A], price, spectrum.classifyClass(price))
      } yield SpectroscopyInterface.SpectroscopyResultItem[S, A, ObservationNotionWeak, HennessyMilnerLogic.Formula[A]](p, q, distinctions.toList, preorders)

      if (saveGameSize) gameSize = hmlGame.gameSize()

      SpectroscopyInterface.SpectroscopyResult[S, A, ObservationNotionWeak, HennessyMilnerLogic.Formula[A]](spectroResults.toList, spectrum)
    }

  }

  def checkIndividualPreorder(comparedPairs: Iterable[(S,S)], notion: String): SpectroscopyInterface.IndividualNotionResult[S] = {
    val hmlGame =
      if (useCleverBranching) {
        new WeakSpectroscopyGameClever(ts, energyCap = 2)
      } else {
        new WeakSpectroscopyGame(ts, energyCap = 2)
      }

    val init = for {
      (p, q) <- comparedPairs
      start <- List(hmlGame.AttackerObservation(p, Set(q)), hmlGame.AttackerObservation(q, Set(p)))
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
      case hmlGame.AttackerObservation(p, qq) if currentEnergy(4) >= Int.MaxValue && qq.size > 1 =>
        // if we have infinitely many immediate conjunctions, use them to chop down blowup on right-hand side
        baseSuccessor.isInstanceOf[hmlGame.DefenderConjunction]
      case hmlGame.AttackerBranchingObservation(p, qq) if useCleverBranching && currentEnergy(4) >= Int.MaxValue && qq.size > 1 =>
        // same as previous case
        baseSuccessor.isInstanceOf[hmlGame.DefenderConjunction]
      case hmlGame.AttackerDelayedObservation(_, _) if (currentEnergy(3) == 0 || currentEnergy(5) == currentEnergy(6)) && baseSuccessor.isInstanceOf[hmlGame.DefenderStableConjunction] =>
        // disregard revival partitions if they make no difference
        baseSuccessor.asInstanceOf[hmlGame.DefenderStableConjunction].qqRevival.isEmpty
      case hmlGame.AttackerDelayedObservation(_, qq) if currentEnergy(1) >= Int.MaxValue && qq.size > 1 =>
        // focus on branching observations if we have infinite supply of them
        !baseSuccessor.isInstanceOf[hmlGame.AttackerObservation]
      case _ => true
    }

    val reachabilityGame: MaterializedEnergyGame[Energy] = new MaterializedEnergyGame[Energy](
      hmlGame, init, notionEnergy, energyUpdate, preferredNodes)

    val attackerWins = reachabilityGame.computeWinningRegion()

    val gameString = debugLog(
      graphvizMaterializedGame(reachabilityGame, attackerWins),
      asLink = "https://edotor.net/?engine=dot#"//"https://dreampuf.github.io/GraphvizOnline/#"
    )

    val relation: Set[(S, String, S)] = for {
      gn <- reachabilityGame.discovered.toSet
      if !attackerWins(gn)
      (p, eString, q) <- gn match {
        case reachabilityGame.MaterializedAttackerPosition(hmlGame.AttackerObservation(p, qq), energy)
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

  def GamePositionToString(
      game: WeakSpectroscopyGame[S, A, L],
      gn: GamePosition) = {
    val str = gn match {
      case game.AttackerObservation(p, qq: Set[_]) =>
        val qqString = qq.mkString("{",",","}")
        s"$p, $qqString"
      case game.AttackerDelayedObservation(p, qq: Set[_]) =>
        val qqString = qq.mkString("{",",","}")
        s"$p, ≈$qqString"
      case game.AttackerBranchingObservation(p, qq: Set[_]) =>
        val qqString = qq.mkString("{",",","}")
        s"$p, b$qqString"
      case game.AttackerClause(p, q) =>
        s"$p, $q"
      case game.DefenderConjunction(p, qq: Set[_]) =>
        val qqString = qq.mkString("{",",","}")
        s"$p, $qqString"
      case game.DefenderStableConjunction(p, qq: Set[_], qqRevivals) =>
        val qqString = qq.mkString("{",",","}")
        val qqRevivalsString = qq.mkString("{",",","}")
        s"$p, s$qqString, $qqRevivalsString"
      case game.DefenderBranchingConjunction(p0, a, p1, qq0, qq0a) =>
        s"$p0 -${a}-> $p1, ${qq0.mkString("{",",","}")}, ${qq0a.mkString("{",",","}")}"
      case _ =>
        if (game.isInstanceOf[WeakSpectroscopyGameClever[S, A, L]]) {
          val gameClever = game.asInstanceOf[WeakSpectroscopyGameClever[S, A, L]]
          gn match {
            case gameClever.AttackerBranchingConjunction(p0, a, p1, q0) =>
              s"$p0 -${a}-> $p1, ${q0}"
            case _ => ""
          }
        } else {
          ""
      }
    }
    str.replaceAllLiterally(".0", "").replaceAllLiterally("\\", "\\\\")
  }

  def graphvizGameWithFormulas(
      game: WeakSpectroscopyGame[S, A, L],
      attackerVictoryPrices: Map[GamePosition, Iterable[Energy]],
      formulas: Map[GamePosition, Set[HennessyMilnerLogic.Formula[A]]]
  ) = {
    val visualizer = new GameGraphVisualizer(game) {

      def nodeToID(gn: GamePosition): String = gn.toString().hashCode().toString()

      def nodeToString(gn: GamePosition): String = {
        val priceString = attackerVictoryPrices.getOrElse(gn,Set()).map(_.vector.mkString("(",",",")")).mkString(" / ")
        val formulaString = formulas.getOrElse(gn,Set()).mkString("\\n").replaceAllLiterally("⟩⊤","⟩")
        GamePositionToString(game, gn) +
         (if (priceString != "") s"\\n------\\n$priceString" else "") +
         (if (formulaString != "") s"\\n------\\n$formulaString" else "")
      }

      def edgeToLabel(gn1: GamePosition, gn2: GamePosition) = game.weight(gn1, gn2).toString()
    }

    val attackerWin = attackerVictoryPrices.filter(_._2.nonEmpty).keySet.toSet

    visualizer.outputDot(attackerWin)
  }

  def materializedToBaseGamePosition(game: MaterializedEnergyGame[Energy], gn: GamePosition) = gn match {
    case game.MaterializedAttackerPosition(bgn, e) =>
      bgn
    case game.MaterializedDefenderPosition(bgn, e) =>
      bgn
  }

  def graphvizMaterializedGame(
      game: MaterializedEnergyGame[Energy],
      attackerWin: Set[GamePosition]
  ) = {
    val baseGame = game.baseGame.asInstanceOf[WeakSpectroscopyGame[S, A, L]]
    val maxIntString = Int.MaxValue.toString()
    val visualizer = new GameGraphVisualizer(game) {

      def nodeToID(gn: GamePosition): String = gn.toString().hashCode().toString()

      def nodeToString(gn: GamePosition): String = gn match {
        case game.MaterializedAttackerPosition(bgn, e) =>
          GamePositionToString(baseGame, bgn) + "\\n" + e.toString().replaceAllLiterally(maxIntString, "∞")
        case game.MaterializedDefenderPosition(bgn, e) =>
          GamePositionToString(baseGame, bgn) + "\\n" + e.toString().replaceAllLiterally(maxIntString, "∞")
      }

      def edgeToLabel(gn1: GamePosition, gn2: GamePosition) = {
        baseGame.weight(materializedToBaseGamePosition(game, gn1), materializedToBaseGamePosition(game, gn2)).toString()
      }

    }

    visualizer.outputDot(attackerWin)
  }
}