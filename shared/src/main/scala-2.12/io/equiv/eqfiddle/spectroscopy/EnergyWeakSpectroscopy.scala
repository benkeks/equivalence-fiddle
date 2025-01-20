package io.equiv.eqfiddle.spectroscopy

import io.equiv.eqfiddle.ts.WeakTransitionSystem
import io.equiv.eqfiddle.algo.AlgorithmLogging
import io.equiv.eqfiddle.hml.ObservationClassFast
import io.equiv.eqfiddle.hml.Spectrum
import io.equiv.eqfiddle.game.SimpleGame
import io.equiv.eqfiddle.game.SimpleGame.GameNode
import io.equiv.eqfiddle.game.EnergyGame
import io.equiv.eqfiddle.game.EnergyGame.Energy
import io.equiv.eqfiddle.hml.HennessyMilnerLogic
import io.equiv.eqfiddle.hml.HMLInterpreter
import io.equiv.eqfiddle.game.GameGraphVisualizer
import io.equiv.eqfiddle.hml.ObservationClassEnergyWeak
import io.equiv.eqfiddle.util.FixedPoint

class EnergyWeakSpectroscopy[S, A, L] (
    ts: WeakTransitionSystem[S, A, L])
  extends SpectroscopyInterface[S, A, L, HennessyMilnerLogic.Formula[A]] with AlgorithmLogging[S] {

  val useCleverBranching: Boolean = true

  val spectrum = ObservationClassEnergyWeak.LTBTS

  val distinguishingFormulas =
    collection.mutable.Map[(GameNode, Energy), Iterable[HennessyMilnerLogic.Formula[A]]]()

  var gameSize = (0, 0)

  def pruneDominated(oldFormulas: Set[HennessyMilnerLogic.Formula[A]]) = {
    spectrum.getLeastDistinguishing(oldFormulas)
  }

  def buildHMLWitness(game: EnergyWeakSpectroscopyGame[S, A, L], node: GameNode, price: Energy): Iterable[HennessyMilnerLogic.Formula[A]]
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
        val gameClever = game.asInstanceOf[EnergyWeakSpectroscopyGameClever[S, A, L]]

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

  private def energyToClass(e: Energy) = {
    ObservationClassEnergyWeak(e(0), e(1), e(2), e(3), e(4), e(5), e(6), e(7), e(8))
  }

  def compute(
      comparedPairs: Iterable[(S,S)]
    ): SpectroscopyInterface.SpectroscopyResult[S, A, ObservationClassEnergyWeak, HennessyMilnerLogic.Formula[A]] = {
    compute(comparedPairs, computeFormulas = true)
  }

  def compute(
      comparedPairs: Iterable[(S,S)],
      computeFormulas: Boolean = true,
      saveGameSize: Boolean = false
    ): SpectroscopyInterface.SpectroscopyResult[S, A, ObservationClassEnergyWeak, HennessyMilnerLogic.Formula[A]] = {

    debugLog(s"Start spectroscopy on ${ts.nodes.size} node transition system with ${comparedPairs.size} compared pairs.")

    val hmlGame = if (useCleverBranching) {
      new EnergyWeakSpectroscopyGameClever(ts, energyCap = if (computeFormulas) Int.MaxValue else 3)
    } else {
      new EnergyWeakSpectroscopyGame(ts, energyCap = if (computeFormulas) Int.MaxValue else 3)
    }

    val init = for {
      (p, q) <- comparedPairs
      start <- List(hmlGame.AttackerObservation(p, Set(q)), hmlGame.AttackerObservation(q, Set(p)))
    } yield start

    val zeroEnergySet = Set(Energy.zeroEnergy(9))

    def instantAttackerWin(gn: GameNode) = gn match {
      case hmlGame.DefenderConjunction(_, qq) if qq.isEmpty => zeroEnergySet
      case _ => Set.empty
    }

    debugLog("HML spectroscopy game construction ...")

    hmlGame.populateGame(
      init,
      (gns => hmlGame.computeSuccessors(gns)),
      instantAttackerWin(_))

    debugLog("HML spectroscopy game size: " + hmlGame.discovered.size)

    if (computeFormulas) {
      for {
        gn <- init
        hmlGame.AttackerObservation(p, qq) = gn
        bestPrice <- hmlGame.attackerVictoryPrices(gn)
        energyClass = energyToClass(bestPrice)
        witnessFormulas = buildHMLWitness(hmlGame, gn, bestPrice)
        //witnessFormulas = potentialWitnesses.filter(f => spectrum.classifyFormula(f)._1 <= energyClass)
        f <- witnessFormulas.headOption
      } {
        val formulaPrice = spectrum.classifyFormula(f)._1
        if (! (formulaPrice <= energyClass) ) {
          System.err.println(s"ERROR: Formula $f ${formulaPrice.toTuple} too expensive; not below ${energyClass.toTuple}.")
        } else {
          if (formulaPrice < energyClass) {
            System.err.println(s"WARNING: Witness formula $f ${formulaPrice.toTuple} is strictly cheaper than determined minimal distinction class ${energyClass.toTuple}!")
          }
          debugLog("Distinguished at " + energyClass.toTuple + ", " + spectrum.classifyClass(energyClass) + " preorder by " + f.toString() + " Price: " + spectrum.classifyFormula(f))
          checkDistinguishing(f, p, qq.head)
        }
      }
      val distinguishingNodeFormulas = for {
        (node, pricedFormulas) <- distinguishingFormulas
          .toSet[((GameNode, Energy), Iterable[HennessyMilnerLogic.Formula[A]])]
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

      val bestPreorders: Map[GameNode,List[Spectrum.EquivalenceNotion[ObservationClassEnergyWeak]]] =
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
      } yield SpectroscopyInterface.SpectroscopyResultItem[S, A, ObservationClassEnergyWeak, HennessyMilnerLogic.Formula[A]](p, q, distinctions, preorders)

      if (saveGameSize) gameSize = hmlGame.gameSize()

      SpectroscopyInterface.SpectroscopyResult[S, A, ObservationClassEnergyWeak, HennessyMilnerLogic.Formula[A]](spectroResults.toList, spectrum, meta = Map("game" -> gameString))
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

      val bestPreorders: Map[GameNode,(Set[ObservationClassEnergyWeak],List[Spectrum.EquivalenceNotion[ObservationClassEnergyWeak]])] =
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
      } yield SpectroscopyInterface.SpectroscopyResultItem[S, A, ObservationClassEnergyWeak, HennessyMilnerLogic.Formula[A]](p, q, distinctions.toList, preorders)

      if (saveGameSize) gameSize = hmlGame.gameSize()

      SpectroscopyInterface.SpectroscopyResult[S, A, ObservationClassEnergyWeak, HennessyMilnerLogic.Formula[A]](spectroResults.toList, spectrum)
    }

  }

  def checkDistinguishing(formula: HennessyMilnerLogic.Formula[A], p: S, q: S) = {
    val hmlInterpreter = new HMLInterpreter(ts)
    val check = hmlInterpreter.isTrueAt(formula, List(p, q))
    if (!check(p) || check(q)) {
      System.err.println("Formula " + formula.toString() + " is no sound distinguishing formula! " + check)
    }
  }

  def graphvizGameWithFormulas(
      game: EnergyWeakSpectroscopyGame[S, A, L],
      attackerVictoryPrices: Map[GameNode, Iterable[Energy]],
      formulas: Map[GameNode, Set[HennessyMilnerLogic.Formula[A]]]
  ) = {
    val visualizer = new GameGraphVisualizer(game) {

      def nodeToID(gn: GameNode): String = gn.toString().hashCode().toString()

      def nodeToString(gn: GameNode): String = {
        val priceString = attackerVictoryPrices.getOrElse(gn,Set()).map(_.vector.mkString("(",",",")")).mkString(" / ")
        val formulaString = formulas.getOrElse(gn,Set()).mkString("\\n").replaceAllLiterally("⟩⊤","⟩")
        (gn match {
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
            if (game.isInstanceOf[EnergyWeakSpectroscopyGameClever[S, A, L]]) {
              val gameClever = game.asInstanceOf[EnergyWeakSpectroscopyGameClever[S, A, L]]
              gn match {
                case gameClever.AttackerBranchingConjunction(p0, a, p1, q0) =>
                  s"$p0 -${a}-> $p1, ${q0}"
                case _ => ""
              }
            } else {
              ""
            }
        }).replaceAllLiterally(".0", "").replaceAllLiterally("\\", "\\\\") +
         (if (priceString != "") s"\\n------\\n$priceString" else "") +
         (if (formulaString != "") s"\\n------\\n$formulaString" else "")
      }

      def edgeToLabel(gn1: GameNode, gn2: GameNode) = game.weight(gn1, gn2).toString()
    }

    val attackerWin = attackerVictoryPrices.filter(_._2.nonEmpty).keySet.toSet

    visualizer.outputDot(attackerWin)
  }
}