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

class EnergyWeakSpectroscopy[S, A, L] (
    ts: WeakTransitionSystem[S, A, L])
  extends SpectroscopyInterface[S, A, L, HennessyMilnerLogic.Formula[A]] with AlgorithmLogging[S] {

  val spectrum = ObservationClassEnergyWeak.LTBTS

  val distinguishingFormulas =
    collection.mutable.Map[(GameNode, Energy), Iterable[HennessyMilnerLogic.Formula[A]]]()

  var gameSize = (0, 0)

  def pruneDominated(oldFormulas: Set[HennessyMilnerLogic.Formula[A]]) = {
    spectrum.getLeastDistinguishing(oldFormulas)
  }

  def buildHMLWitness(game: EnergyWeakSpectroscopyGame[S, A, L], node: GameNode, price: Energy): Iterable[HennessyMilnerLogic.Formula[A]]
    = distinguishingFormulas.getOrElseUpdate((node, price), {
    //debugLog(s"exploring: $node, $price" )
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
      case game.AttackerDelayedObservation(p0, qq0) =>
        val successorFormulas =
          for {
            s <- game.successors(node)
            update = game.weight(node, s)
            newPrice = update.applyEnergyUpdate(price)
            if game.isAttackerWinningPrice(s, newPrice)
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
            case game.AttackerDelayedObservation(p1, qq1) =>
              buildHMLWitness(game, s, newPrice)
            case game.DefenderConjunction(p1, qq1) =>
              buildHMLWitness(game, s, newPrice)
            case game.DefenderStableConjunction(p1, qq1) =>
              buildHMLWitness(game, s, newPrice)
            case game.DefenderBranchingConjunction(p01, a, p1, qq01, qq01a) =>
              buildHMLWitness(game, s, newPrice)
            case _ => Set()
          }
        //pruneDominated(successorFormulas.flatten.toSet)
        successorFormulas.headOption.flatMap(_.headOption)
      case game.AttackerClause(p0, q0) =>
        val successorFormulas = for {
          s <- game.successors(node)
          update = game.weight(node, s)
          newPrice = update.applyEnergyUpdate(price)
          if game.isAttackerWinningPrice(s, newPrice)
        } yield {
          s match {
            case game.AttackerObservation(p1, qq1) =>
              if (p0 == p1) {
                buildHMLWitness(game, s, newPrice)
              } else {
                for {
                  postForm <- buildHMLWitness(game, s, newPrice)
                } yield HennessyMilnerLogic.Negate(postForm)
              }
            }
          }
        successorFormulas.flatten
      case game.DefenderBranchingConjunction(p0, a, p1, qq0, qq0a) =>
        val aBranches = for {
          s <- game.successors(node)
          if s.isInstanceOf[game.AttackerObservation]
          update = game.weight(node, s)
          newPrice = update.applyEnergyUpdate(price)
          if game.isAttackerWinningPrice(s, newPrice)
          subformula <- buildHMLWitness(game, s, newPrice)
        } yield {
          s match {
            case game.AttackerObservation(_, _) if ts.silentActions(a) =>
              HennessyMilnerLogic.ObserveInternal(subformula, opt = true)
            case game.AttackerObservation(_, _) =>
              HennessyMilnerLogic.Observe(a, subformula)
            case _ =>
              subformula
          }
        }
        val possibleMoves = (for {
          s <- game.successors(node)
          update = game.weight(node, s)
          newPrice = update.applyEnergyUpdate(price)
          if !s.isInstanceOf[game.AttackerObservation]
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
      case game.DefenderConjunction(_, _) | game.DefenderStableConjunction(_, _) =>
        val possibleMoves = for {
          s <- game.successors(node)
          update = game.weight(node, s)
          newPrice = update.applyEnergyUpdate(price)
          if !s.isInstanceOf[game.DefenderConjunction]
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

    val hmlGame = new EnergyWeakSpectroscopyGame(ts, energyCap = if (computeFormulas) Int.MaxValue else 3)

    val init = for {
      (p, q) <- comparedPairs
      start <- List(hmlGame.AttackerObservation(p, Set(q)), hmlGame.AttackerObservation(q, Set(p)))
    } yield start

    val zeroEnergySet = Set(Energy.zeroEnergy(9))

    def instantAttackerWin(gn: GameNode) = gn match {
      case hmlGame.DefenderConjunction(_, qq) if qq.isEmpty => zeroEnergySet
      case hmlGame.DefenderStableConjunction(_, qq) if qq.isEmpty => zeroEnergySet
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
          !hmlGame.attackerVictoryPrices.isDefinedAt(gn)
      } yield (gn, Set[HennessyMilnerLogic.Formula[A]]())

      val distinguishingNodeFormulasExtended = distinguishingNodeFormulas ++ bisimilarNodes

      debugLog(
        graphvizGameWithFormulas(hmlGame, hmlGame.attackerVictoryPrices.toMap, distinguishingNodeFormulasExtended),
        asLink = "https://dreampuf.github.io/GraphvizOnline/#"
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

      SpectroscopyInterface.SpectroscopyResult[S, A, ObservationClassEnergyWeak, HennessyMilnerLogic.Formula[A]](spectroResults.toList, spectrum)
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
          !hmlGame.attackerVictoryPrices.isDefinedAt(gn)
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
          case game.AttackerClause(p, q) =>
            s"$p, $q"
          // case game.AttackerBranchingClause(p0, a, p1, q) =>
          //   s"$p0 -${a}-> $p1, $q"
          case game.DefenderConjunction(p, qq: Set[_]) =>
            val qqString = qq.mkString("{",",","}")
            s"$p, $qqString"
          case game.DefenderStableConjunction(p, qq: Set[_]) =>
            val qqString = qq.mkString("{",",","}")
            s"$p, s$qqString"
          case game.DefenderBranchingConjunction(p0, a, p1, qq0, qq0a) =>
            s"$p0 -${a}-> $p1, ${qq0.mkString("{",",","}")}, ${qq0a.mkString("{",",","}")}}"
          case _ => ""
        }).replaceAllLiterally(".0", "") +
         (if (priceString != "") s"\\n------\\n$priceString" else "") +
         (if (formulaString != "") s"\\n------\\n$formulaString" else "")
      }

      def edgeToLabel(gn1: GameNode, gn2: GameNode) = game.weight(gn1, gn2).toString()
    }

    val attackerWin = attackerVictoryPrices.filter(_._2.nonEmpty).keySet.toSet

    visualizer.outputDot(attackerWin)
  }
}