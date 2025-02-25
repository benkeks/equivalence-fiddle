package io.equiv.eqfiddle.spectroscopy

import io.equiv.eqfiddle.ts.WeakTransitionSystem
import io.equiv.eqfiddle.algo.AlgorithmLogging
import io.equiv.eqfiddle.hml.ObservationNotionStrong
import io.equiv.eqfiddle.hml.Spectrum
import io.equiv.eqfiddle.game.SimpleGame
import io.equiv.eqfiddle.game.SimpleGame.GameNode
import io.equiv.eqfiddle.game.EnergyGame
import io.equiv.eqfiddle.game.EnergyGame.Energy
import io.equiv.eqfiddle.game.MaterializedEnergyGame
import io.equiv.eqfiddle.hml.HennessyMilnerLogic
import io.equiv.eqfiddle.hml.HMLInterpreter
import io.equiv.eqfiddle.game.GameGraphVisualizer

class StrongSpectroscopy[S, A, L] (
    ts: WeakTransitionSystem[S, A, L])
  extends SpectroscopyInterface[S, A, L, HennessyMilnerLogic.Formula[A]] with AlgorithmLogging[S] {

  val spectrum = ObservationNotionStrong.LTBTS

  val useCleverSpectroscopyGame: Boolean = true

  val distinguishingFormulas =
    collection.mutable.Map[(GameNode, Energy), Iterable[HennessyMilnerLogic.Formula[A]]]()

  var gameSize = (0, 0)

  private def classToEnergy(obsClass: ObservationNotionStrong): Energy = {
    val c = obsClass.toTuple
    Energy(Array(c._1, c._2, c._3, c._4, c._5, c._6))
  }


  def buildHMLWitness(game: StrongSpectroscopyGame[S, A, L], node: GameNode, price: Energy): Iterable[HennessyMilnerLogic.Formula[A]]
    = distinguishingFormulas.getOrElseUpdate((node, price), {
    node match {
      case game.AttackerObservation(p0, qq0) if qq0.isEmpty =>
        Set(HennessyMilnerLogic.True)
      case game.AttackerObservation(p0, qq0) =>
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
            case game.DefenderConjunction(_, _, _) =>
              buildHMLWitness(game, s, newPrice)
            case _ => Set()
          }
        successorFormulas.flatten.toSet
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
      case game.DefenderConjunction(_, _, _) =>
        val possibleMoves = for {
          s <- game.successors(node)
          update = game.weight(node, s)
          newPrice = update.applyEnergyUpdate(price)
        } yield if (game.isAttackerWinningPrice(s, newPrice)) {
          buildHMLWitness(game, s, newPrice)
        } else {
          Set()
        }
        val productMoves =
          possibleMoves.foldLeft(Seq(Seq[HennessyMilnerLogic.Formula[A]]()))(
            (b, a) => b.flatMap(i => a.map(j => i ++ Seq(j))))
        productMoves.map { mv =>
          val moves = mv.toSet
          HennessyMilnerLogic.And(moves).asInstanceOf[HennessyMilnerLogic.Formula[A]]
        }
    }
  })

  def compute(
      comparedPairs: Iterable[(S,S)]
    ): SpectroscopyInterface.SpectroscopyResult[S, A, ObservationNotionStrong, HennessyMilnerLogic.Formula[A]] = {
    compute(comparedPairs, computeFormulas = true)
  }

  def compute(
      comparedPairs: Iterable[(S,S)],
      computeFormulas: Boolean = true,
      saveGameSize: Boolean = false
    ): SpectroscopyInterface.SpectroscopyResult[S, A, ObservationNotionStrong, HennessyMilnerLogic.Formula[A]] = {


    debugLog(s"Start spectroscopy on ${ts.nodes.size} node transition system with ${comparedPairs.size} compared pairs.")

    val hmlGame = new StrongSpectroscopyGame(ts, energyCap = if (computeFormulas) Int.MaxValue else 3) {
      override val optimizeConjMoves: Boolean = useCleverSpectroscopyGame
    }

    val init = for {
      (p, q) <- comparedPairs
      start <- List(hmlGame.AttackerObservation(p, Set(q)), hmlGame.AttackerObservation(q, Set(p)))
    } yield start

    val zeroEnergySet = Set(Energy.zeroEnergy(6))

    def instantAttackerWin(gn: GameNode) = gn match {
      case hmlGame.DefenderConjunction(_, qqS, qqR) if qqS.isEmpty && qqR.isEmpty => zeroEnergySet; case _ => Set.empty
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
        witnessFormula <- buildHMLWitness(hmlGame, gn, bestPrice)
      } {
        debugLog("Distinguished under " + spectrum.classifyFormula(witnessFormula) + " preorder by " + witnessFormula.toString())
        checkDistinguishing(witnessFormula, p, qq.head)
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
        asLink = "https://edotor.net/?engine=dot#"
      )

      val bestPreorders: Map[GameNode,List[Spectrum.EquivalenceNotion[ObservationNotionStrong]]] =
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
      } yield SpectroscopyInterface.SpectroscopyResultItem[S, A, ObservationNotionStrong, HennessyMilnerLogic.Formula[A]](p, q, distinctions, preorders)

      if (saveGameSize) gameSize = hmlGame.gameSize()

      SpectroscopyInterface.SpectroscopyResult[S, A, ObservationNotionStrong, HennessyMilnerLogic.Formula[A]](spectroResults.toList, spectrum, meta = Map("game" -> gameString))
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

      val bestPreorders: Map[GameNode,(Set[ObservationNotionStrong],List[Spectrum.EquivalenceNotion[ObservationNotionStrong]])] =
        hmlGame.attackerVictoryPrices.toMap.mapValues { energies =>
        val fcs = energies.toSet[Energy].map(e => ObservationNotionStrong(e(0), e(1), e(2), e(3), e(4), e(5)))
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
      } yield SpectroscopyInterface.SpectroscopyResultItem[S, A, ObservationNotionStrong, HennessyMilnerLogic.Formula[A]](p, q, distinctions.toList, preorders)

      if (saveGameSize) gameSize = hmlGame.gameSize()

      SpectroscopyInterface.SpectroscopyResult[S, A, ObservationNotionStrong, HennessyMilnerLogic.Formula[A]](spectroResults.toList, spectrum)
    }

  }

  def checkDistinguishing(formula: HennessyMilnerLogic.Formula[A], p: S, q: S) = {
    val hmlInterpreter = new HMLInterpreter(ts)
    val check = hmlInterpreter.isTrueAt(formula, List(p, q))
    if (!check(p) || check(q)) {
      AlgorithmLogging.debugLog("Formula " + formula.toString() + " is no sound distinguishing formula! " + check, logLevel = 4)
    }
  }

  def graphvizGameWithFormulas(
      game: StrongSpectroscopyGame[S, A, L],
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
          case game.AttackerClause(p, q) =>
            s"$p, $q"
          case game.DefenderConjunction(p, qqS: Set[_], qqR: Set[_]) =>
            val qqSString = qqS.mkString("{",",","}")
            val qqRString = qqR.mkString("{",",","}")
            s"$p, $qqSString, $qqRString"
          case _ => ""
        }).replaceAllLiterally(".0", "").replaceAllLiterally("\\", "\\\\") +
         (if (priceString != "") s"\\n------\\n$priceString" else "") +
         (if (formulaString != "") s"\\n------\\n$formulaString" else "")
      }

      def edgeToLabel(gn1: GameNode, gn2: GameNode) = game.weight(gn1, gn2).toString()
    }

    val attackerWin = attackerVictoryPrices.filter(_._2.nonEmpty).keySet.toSet

    visualizer.outputDot(attackerWin)
  }

  def checkIndividualPreorder(comparedPairs: Iterable[(S,S)], notion: String): SpectroscopyInterface.IndividualNotionResult[S] = {
    val hmlGame = new StrongSpectroscopyGame(ts, energyCap = 3)

    val init = for {
      (p, q) <- comparedPairs
      start <- List(hmlGame.AttackerObservation(p, Set(q)), hmlGame.AttackerObservation(q, Set(p)))
    } yield start

    val notionEnergy = classToEnergy(spectrum.getSpectrumClass(notion).obsClass)

    def energyUpdate(gn1: GameNode, gn2: GameNode, energy: Energy): Option[Energy] = {
      val update = hmlGame.weight(gn1, gn2)
      val newEnergy = update.applyEnergyUpdateInfinity(energy)
      if (gn1.isInstanceOf[SimpleGame.DefenderNode] || newEnergy.isNonNegative())
        Some(newEnergy)
      else
        None
    }

    // whether to consider the baseSuccessor as a relevant node for the attacker
    def preferredNodes(currentBaseNode: GameNode, currentEnergy: Energy, baseSuccessor: GameNode) = currentBaseNode match {
      case hmlGame.AttackerObservation(p, qq) if currentEnergy(1) >= Int.MaxValue && qq.size > 1 =>
        // if we have infinitely many immediate conjunctions, use them to chop down blowup on right-hand side
        baseSuccessor.isInstanceOf[hmlGame.DefenderConjunction]
      case _ => true
    }

    val reachabilityGame: MaterializedEnergyGame[Energy] = new MaterializedEnergyGame[Energy](
      hmlGame, init, notionEnergy, energyUpdate, preferredNodes)

    val attackerWins = reachabilityGame.computeWinningRegion()

    val relation: Set[(S, String, S)] = for {
      gn <- reachabilityGame.discovered.toSet
      if !attackerWins(gn)
      (p, eString, q) <- gn match {
        case reachabilityGame.MaterializedAttackerNode(hmlGame.AttackerObservation(p, qq), energy)
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
    SpectroscopyInterface.IndividualNotionResult(items, relation)
  }

}
