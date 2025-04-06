package io.equiv.eqfiddle.spectroscopy

import io.equiv.eqfiddle.ts.WeakTransitionSystem
import io.equiv.eqfiddle.algo.AlgorithmLogging
import io.equiv.eqfiddle.hml.ObservationNotionStrong
import io.equiv.eqfiddle.hml.Spectrum
import io.equiv.eqfiddle.game.SimpleGame
import io.equiv.eqfiddle.game.EnergyGame
import io.equiv.eqfiddle.game.EnergyGame.Energy
import io.equiv.eqfiddle.game.MaterializedEnergyGame
import io.equiv.eqfiddle.hml.HennessyMilnerLogic
import io.equiv.eqfiddle.hml.HMLInterpreter
import io.equiv.eqfiddle.game.GameGraphVisualizer


class StrongSpectroscopy[S, A, L] (
    override val ts: WeakTransitionSystem[S, A, L])
  extends SpectroscopyInterface[S, A, L, HennessyMilnerLogic.Formula[A]] with AlgorithmLogging[S] {

  import StrongSpectroscopyGame._
  import MaterializedEnergyGame._

  type Notion = ObservationNotionStrong
  type SpectroscopyGame = StrongSpectroscopyGame[S, A, L]
  type GamePosition = StrongSpectroscopyGamePosition[S]

  val spectrum = ObservationNotionStrong.LTBTS

  override def notionToEnergy(obsNotion: Notion): Energy = {
    val c = obsNotion.toTuple
    Energy(Array(c._1, c._2, c._3, c._4, c._5, c._6))
  }
  override def energyToNotion(energy: Energy): Notion = {
    ObservationNotionStrong(energy(0), energy(1), energy(2), energy(3), energy(4), energy(5))
  }

  def buildHMLWitness(game: SpectroscopyGame, node: GamePosition, price: Energy): Iterable[HennessyMilnerLogic.Formula[A]]
    = distinguishingFormulas.getOrElseUpdate((node, price), {
    node match {
      case AttackerObservation(p0, qq0) if qq0.isEmpty =>
        Set(HennessyMilnerLogic.True)
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
              } yield HennessyMilnerLogic.Observe(a, postForm)
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
                } yield HennessyMilnerLogic.Negate(postForm)
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
    ): SpectroscopyInterface.SpectroscopyResult[S, A, Notion, HennessyMilnerLogic.Formula[A]] = {
    compute(comparedPairs, SpectroscopyInterface.SpectroscopyConfig())
  }

  def compute(
      comparedPairs: Iterable[(S,S)],
      config: SpectroscopyInterface.SpectroscopyConfig
    ): SpectroscopyInterface.SpectroscopyResult[S, A, Notion, HennessyMilnerLogic.Formula[A]] = {

    debugLog(s"Start spectroscopy on ${ts.nodes.size} node transition system with ${comparedPairs.size} compared pairs.")

    val hmlGame = new StrongSpectroscopyGame(ts, config)

    val init = for {
      (p, q) <- comparedPairs
      start <- List(AttackerObservation(p, Set(q)), AttackerObservation(q, Set(p)))
    } yield start

    val zeroEnergySet = Set(Energy.zeroEnergy(6))

    def instantAttackerWin(gn: GamePosition) = gn match {
      case DefenderConjunction(_, qqS, qqR) if qqS.isEmpty && qqR.isEmpty => zeroEnergySet; case _ => Set.empty
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
        if (gn match { case AttackerObservation(_, qq) => qq.size == 1; case _ => false }) &&
          (!hmlGame.attackerWinningBudgets.isDefinedAt(gn) || hmlGame.attackerWinningBudgets(gn).isEmpty)
      } yield (gn, Set[HennessyMilnerLogic.Formula[A]]())

      val distinguishingNodeFormulasExtended = distinguishingNodeFormulas ++ bisimilarNodes

      val gameString = debugLog(
        graphvizGameWithFormulas(hmlGame, hmlGame.attackerWinningBudgets.toMap, distinguishingNodeFormulasExtended),
        asLink = "https://edotor.net/?engine=dot#"
      )

      val bestPreorders: Map[GamePosition,List[Spectrum.EquivalenceNotion[Notion]]] =
        distinguishingNodeFormulasExtended.mapValues { ffs =>
        val classes = ffs.flatMap(spectrum.classifyFormula(_)._2)
        spectrum.getStrongestPreorderClass(classes)
      }

      val spectroResults = for {
        gn <- hmlGame.discovered
        if gn.isInstanceOf[AttackerObservation[S]]
        AttackerObservation(p, qq) = gn
        if qq.size == 1
        q <- qq
        preorders <- bestPreorders.get(gn)
        distinctionFormulas = distinguishingNodeFormulasExtended(gn)
        distinctions = for {
          f <- distinctionFormulas.toList
          (price, eqs) = spectrum.classifyFormula(f)
        } yield (f, price, eqs)
      } yield SpectroscopyInterface.SpectroscopyResultItem[S, A, Notion, HennessyMilnerLogic.Formula[A]](p, q, distinctions, preorders)

      if (config.saveGameSize) gameSize = hmlGame.gameSize()

      SpectroscopyInterface.SpectroscopyResult[S, A, Notion, HennessyMilnerLogic.Formula[A]](spectroResults.toList, spectrum, meta = Map("game" -> gameString))
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

      val bestPreorders: Map[GamePosition,(Set[Notion],List[Spectrum.EquivalenceNotion[Notion]])] =
        hmlGame.attackerWinningBudgets.toMap.mapValues { energies =>
        val fcs = energies.toSet[Energy].map(energyToNotion(_))
        (fcs, spectrum.getStrongestPreorderClassFromClass(fcs))
      }

      val spectroResults = for {
        gn <- hmlGame.discovered
        if gn.isInstanceOf[AttackerObservation[S]]
        AttackerObservation(p, qq) = gn
        if qq.size == 1
        q <- qq
        (prices, preorders) <- bestPreorders.get(gn)
        distinctions = for {
          price <- prices
        } yield (HennessyMilnerLogic.True[A], price, spectrum.classifyClass(price))
      } yield SpectroscopyInterface.SpectroscopyResultItem[S, A, Notion, HennessyMilnerLogic.Formula[A]](p, q, distinctions.toList, preorders)

      if (config.saveGameSize) gameSize = hmlGame.gameSize()

      SpectroscopyInterface.SpectroscopyResult[S, A, Notion, HennessyMilnerLogic.Formula[A]](spectroResults.toList, spectrum)
    }

  }

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

  def checkIndividualPreorder(
      comparedPairs: Iterable[(S,S)],
      notion: String,
      config: SpectroscopyInterface.SpectroscopyConfig = SpectroscopyInterface.SpectroscopyConfig()
  ) : SpectroscopyInterface.IndividualNotionResult[S] = {
    val hmlGame = new StrongSpectroscopyGame(ts, config)

    val init = for {
      (p, q) <- comparedPairs
      start <- List(AttackerObservation(p, Set(q)), AttackerObservation(q, Set(p)))
    } yield start

    val notionEnergy = notionToEnergy(spectrum.getSpectrumClass(notion).obsNotion)

    def energyUpdate(gn1: GamePosition, gn2: GamePosition, energy: Energy): Option[Energy] = {
      val update = hmlGame.weight(gn1, gn2)
      val newEnergy = update.applyEnergyUpdateInfinity(energy)
      if (gn1.isInstanceOf[SimpleGame.DefenderPosition] || newEnergy.isNonNegative())
        Some(newEnergy)
      else
        None
    }

    // whether to consider the baseSuccessor as a relevant node for the attacker
    def preferredNodes(currentBaseNode: GamePosition, currentEnergy: Energy, baseSuccessor: GamePosition): Boolean = {
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

    val reachabilityGame: MaterializedEnergyGame[GamePosition, Energy] = new MaterializedEnergyGame[GamePosition, Energy](
      hmlGame, init, notionEnergy, energyUpdate, if (config.useCleverInstanceBranching) preferredNodes else ((_ ,_ ,_ ) => true))

    val attackerWins = reachabilityGame.computeWinningRegion()
    if (config.saveGameSize) gameSize = reachabilityGame.gameSize()

    val gameString = debugLog(
      graphvizMaterializedGame(reachabilityGame, attackerWins),
      asLink = "https://edotor.net/?engine=dot#"
    )

    val relation: Set[(S, String, S)] = for {
      gn <- reachabilityGame.discovered.toSet
      if !attackerWins(gn)
      (p, eString, q) <- gn match {
        case MaterializedAttackerPosition(AttackerObservation(p, qq), energy)
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

  def graphvizMaterializedGame(
      game: MaterializedEnergyGame[GamePosition, Energy],
      attackerWin: Set[MaterializedPosition]
  ) = {
    val baseGame = game.baseGame.asInstanceOf[SpectroscopyGame]
    val maxIntString = Int.MaxValue.toString()
    val visualizer = new GameGraphVisualizer(game) {

      def positionToID(gn: MaterializedPosition): String = gn.hashCode().toString()

      def positionToString(gn: MaterializedPosition): String = gn match {
        case MaterializedAttackerPosition(bgn, e) =>
          gamePositionToString(bgn) + "\\n" + e.toString().replaceAllLiterally(maxIntString, "∞")
        case MaterializedDefenderPosition(bgn, e) =>
          gamePositionToString(bgn) + "\\n" + e.toString().replaceAllLiterally(maxIntString, "∞")
      }

      def moveToLabel(gn1: MaterializedPosition, gn2: MaterializedPosition) = {
        baseGame.weight(materializedToBaseGamePosition(gn1), materializedToBaseGamePosition(gn2)).toString()
      }

    }

    visualizer.outputDot(attackerWin)
  }

}
