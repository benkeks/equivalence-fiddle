package io.equiv.eqfiddle.spectroscopy

import io.equiv.eqfiddle.ts.WeakTransitionSystem
import io.equiv.eqfiddle.algo.AlgorithmLogging
import io.equiv.eqfiddle.hml.ObservationNotionStrong
import io.equiv.eqfiddle.hml.Spectrum
import io.equiv.eqfiddle.game.SimpleGame
import io.equiv.eqfiddle.game.SimpleGame.GamePosition
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

  val distinguishingFormulas =
    collection.mutable.Map[(GamePosition, Energy), Iterable[HennessyMilnerLogic.Formula[A]]]()

  var gameSize = (0, 0)

  private def classToEnergy(obsClass: ObservationNotionStrong): Energy = {
    val c = obsClass.toTuple
    Energy(Array(c._1, c._2, c._3, c._4, c._5, c._6))
  }


  def buildHMLWitness(game: StrongSpectroscopyGame[S, A, L], node: GamePosition, price: Energy): Iterable[HennessyMilnerLogic.Formula[A]]
    = distinguishingFormulas.getOrElseUpdate((node, price), {
    node match {
      case game.AttackerObservation(p0, qq0) if qq0.isEmpty =>
        Set(HennessyMilnerLogic.True)
      case game.AttackerObservation(p0, qq0) =>
        val successorFormulas =
          for {
            s <- game.successors(node)
            update = game.weight(node, s)
            newBudget = update.applyEnergyUpdate(price)
            if game.isAttackerWinningEnergy(s, newBudget)
          } yield s match {
            case game.AttackerObservation(p1, qq1) =>
              val possibleRestoredActions = for {
                (a, pp1) <- ts.post(p0)
                if pp1 contains p1
                if qq1 == ts.post(qq0,a)
              } yield a
              for {
                a <- possibleRestoredActions.headOption.toList // just take first option
                postForm <- buildHMLWitness(game, s, newBudget)
              } yield HennessyMilnerLogic.Observe(a, postForm)
            case game.DefenderConjunction(_, _, _) =>
              buildHMLWitness(game, s, newBudget)
            case _ => Set()
          }
        successorFormulas.flatten.toSet
      case game.AttackerClause(p0, q0) =>
        val successorFormulas = for {
          s <- game.successors(node)
          update = game.weight(node, s)
          newBudget = update.applyEnergyUpdate(price)
          if game.isAttackerWinningEnergy(s, newBudget)
        } yield {
          s match {
            case game.AttackerObservation(p1, qq1) =>
              if (p0 == p1) {
                buildHMLWitness(game, s, newBudget)
              } else {
                for {
                  postForm <- buildHMLWitness(game, s, newBudget)
                } yield HennessyMilnerLogic.Negate(postForm)
              }
            }
          }
        successorFormulas.flatten
      case game.DefenderConjunction(_, _, _) =>
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
    compute(comparedPairs, SpectroscopyInterface.SpectroscopyConfig())
  }

  def compute(
      comparedPairs: Iterable[(S,S)],
      config: SpectroscopyInterface.SpectroscopyConfig
    ): SpectroscopyInterface.SpectroscopyResult[S, A, ObservationNotionStrong, HennessyMilnerLogic.Formula[A]] = {


    debugLog(s"Start spectroscopy on ${ts.nodes.size} node transition system with ${comparedPairs.size} compared pairs.")

    val hmlGame = new StrongSpectroscopyGame(ts, config)

    val init = for {
      (p, q) <- comparedPairs
      start <- List(hmlGame.AttackerObservation(p, Set(q)), hmlGame.AttackerObservation(q, Set(p)))
    } yield start

    val zeroEnergySet = Set(Energy.zeroEnergy(6))

    def instantAttackerWin(gn: GamePosition) = gn match {
      case hmlGame.DefenderConjunction(_, qqS, qqR) if qqS.isEmpty && qqR.isEmpty => zeroEnergySet; case _ => Set.empty
    }

    debugLog("HML spectroscopy game construction ...")

    hmlGame.populateGame(
      init,
      instantAttackerWin(_))

    debugLog("HML spectroscopy game size: " + hmlGame.discovered.size)

    if (config.computeFormulas) {
      for {
        gn <- init
        hmlGame.AttackerObservation(p, qq) = gn
        bestPrice <- hmlGame.attackerWinningBudgets(gn)
        witnessFormula <- buildHMLWitness(hmlGame, gn, bestPrice)
      } {
        debugLog("Distinguished under " + spectrum.classifyFormula(witnessFormula) + " preorder by " + witnessFormula.toString())
        checkDistinguishing(witnessFormula, p, qq.head)
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
          (!hmlGame.attackerWinningBudgets.isDefinedAt(gn) || hmlGame.attackerWinningBudgets(gn).isEmpty)
      } yield (gn, Set[HennessyMilnerLogic.Formula[A]]())

      val distinguishingNodeFormulasExtended = distinguishingNodeFormulas ++ bisimilarNodes

      val gameString = debugLog(
        graphvizGameWithFormulas(hmlGame, hmlGame.attackerWinningBudgets.toMap, distinguishingNodeFormulasExtended),
        asLink = "https://edotor.net/?engine=dot#"
      )

      val bestPreorders: Map[GamePosition,List[Spectrum.EquivalenceNotion[ObservationNotionStrong]]] =
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

      if (config.saveGameSize) gameSize = hmlGame.gameSize()

      SpectroscopyInterface.SpectroscopyResult[S, A, ObservationNotionStrong, HennessyMilnerLogic.Formula[A]](spectroResults.toList, spectrum, meta = Map("game" -> gameString))
    } else {
      for {
        gn <- init
        hmlGame.AttackerObservation(p, qq) = gn
      } {
        hmlGame.attackerWinningBudgets(gn)
      }

      // handle bisimilar nodes
      val bisimilarNodes = for {
        gn <- hmlGame.discovered
        if (gn match { case hmlGame.AttackerObservation(_, qq) => qq.size == 1; case _ => false }) &&
          (!hmlGame.attackerWinningBudgets.isDefinedAt(gn) || hmlGame.attackerWinningBudgets(gn).isEmpty)
      } {
        hmlGame.attackerWinningBudgets(gn) = List()
      }

      val bestPreorders: Map[GamePosition,(Set[ObservationNotionStrong],List[Spectrum.EquivalenceNotion[ObservationNotionStrong]])] =
        hmlGame.attackerWinningBudgets.toMap.mapValues { energies =>
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

      if (config.saveGameSize) gameSize = hmlGame.gameSize()

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
      attackerWinningBudgets: Map[GamePosition, Iterable[Energy]],
      formulas: Map[GamePosition, Set[HennessyMilnerLogic.Formula[A]]]
  ) = {
    val visualizer = new GameGraphVisualizer(game) {

      def positionToID(gn: GamePosition): String =
        gn.toString().hashCode().toString().replace("-", "n")

      def positionToString(gn: GamePosition): String = {
        val budgetString = attackerWinningBudgets.getOrElse(gn,Set()).map(_.vector.mkString("(",",",")")).mkString(" / ")
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
          case _ => "ERROR"
        }).replaceAllLiterally(".0", "").replaceAllLiterally("\\", "\\\\") +
         (if (budgetString != "") s"\\n------\\n$budgetString" else "") +
         (if (formulaString != "") s"\\n------\\n$formulaString" else "")
      }

      def moveToLabel(gn1: GamePosition, gn2: GamePosition) = game.weight(gn1, gn2).toString()
    }

    val attackerWin = attackerWinningBudgets.filter(_._2.nonEmpty).keySet.toSet

    visualizer.outputDot(attackerWin)
  }

  def checkIndividualPreorder(
      comparedPairs: Iterable[(S,S)],
      notion: String,
      config: SpectroscopyInterface.SpectroscopyConfig = SpectroscopyInterface.SpectroscopyConfig()
  ) : SpectroscopyInterface.IndividualNotionResult[S] = {
    val hmlGame = new StrongSpectroscopyGame(ts, config)

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
      case hmlGame.AttackerObservation(p, qq) if currentEnergy(1) >= Int.MaxValue && qq.size > 1 =>
        // if we have infinitely many immediate conjunctions, use them to chop down blowup on right-hand side
        baseSuccessor.isInstanceOf[hmlGame.DefenderConjunction]
      case _ => true
    }

    val reachabilityGame: MaterializedEnergyGame[Energy] = new MaterializedEnergyGame[Energy](
      hmlGame, init, notionEnergy, energyUpdate, preferredNodes)

    val attackerWins = reachabilityGame.computeWinningRegion()
    if (config.saveGameSize) gameSize = reachabilityGame.gameSize()

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
    SpectroscopyInterface.IndividualNotionResult(items, relation)
  }

}
