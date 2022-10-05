package de.bbisping.eqfiddle.spectroscopy

import de.bbisping.eqfiddle.ts.WeakTransitionSystem
import de.bbisping.eqfiddle.algo.AlgorithmLogging
import de.bbisping.eqfiddle.hml.ObservationClassFast
import de.bbisping.eqfiddle.hml.Spectrum
import de.bbisping.eqfiddle.game.SimpleGame
import de.bbisping.eqfiddle.game.SimpleGame.GameNode
import de.bbisping.eqfiddle.game.EnergyGame
import de.bbisping.eqfiddle.game.EnergyGame.Energy
import de.bbisping.eqfiddle.hml.HennessyMilnerLogic
import de.bbisping.eqfiddle.hml.HMLInterpreter
import de.bbisping.eqfiddle.game.GameGraphVisualizer

class FastSpectroscopy[S, A, L] (
    ts: WeakTransitionSystem[S, A, L])
  extends AlgorithmLogging[S] {

  val spectrum = ObservationClassFast.LTBTS

  val distinguishingFormulas =
    collection.mutable.Map[(GameNode, Energy), Set[HennessyMilnerLogic.Formula[A]]]()

  def buildHMLWitness(game: EnergySpectroscopyGame[S, A, L], node: GameNode, price: Energy): Set[HennessyMilnerLogic.Formula[A]]
    = distinguishingFormulas.getOrElseUpdate((node, price), {
    //debugLog(s"exploring: $node, $price" )
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
                a <- possibleRestoredActions.headOption.toSet[A] // just take first option
                postForm <- buildHMLWitness(game, s, newPrice)
              } yield HennessyMilnerLogic.Observe(a, postForm)
            case game.DefenderConjunction(p1, qq1) =>
              buildHMLWitness(game, s, newPrice)
            case _ => Set()
          }
        successorFormulas.toSet.flatten
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
        successorFormulas.toSet.flatten
      case game.DefenderConjunction(_, _) =>
        val possibleMoves = for {
          s <- game.successors(node)
          update = game.weight(node, s)
          newPrice = update.applyEnergyUpdate(price)
        } yield if (game.isAttackerWinningPrice(s, newPrice)) {
          buildHMLWitness(game, s, price)
        } else {
          Set()
        }
        val productMoves =
          possibleMoves.foldLeft(Seq(Seq[HennessyMilnerLogic.Formula[A]]()))(
            (b, a) => b.flatMap(i => a.map(j => i ++ Seq(j))))
        productMoves.map { mv =>
          val moves = mv.toSet
          HennessyMilnerLogic.And(moves).asInstanceOf[HennessyMilnerLogic.Formula[A]]
        }.toSet
    }
  })

  def compute(
      comparedPairs: Iterable[(S,S)],
      computeFormulas: Boolean = true
    ): AbstractSpectroscopy.SpectroscopyResult[S, A, ObservationClassFast, HennessyMilnerLogic.Formula[A]] = {


    debugLog(s"Start spectroscopy on ${ts.nodes.size} node transition system with ${comparedPairs.size} compared pairs.")

    val hmlGame = new EnergySpectroscopyGame(ts)

    val init = for {
      (p, q) <- comparedPairs
      start <- List(hmlGame.AttackerObservation(p, Set(q)), hmlGame.AttackerObservation(q, Set(p)))
    } yield start

    val zeroEnergySet = Set(EnergyGame.zeroEnergy(4))

    def instantAttackerWin(gn: GameNode) = gn match {
      case hmlGame.DefenderConjunction(_, qq) if qq.isEmpty => zeroEnergySet; case _ => Set.empty
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
        witnessFormula <- buildHMLWitness(hmlGame, gn, bestPrice)
      } {
        debugLog("Distinguished under " + spectrum.classifyFormula(witnessFormula) + " preorder by " + witnessFormula.toString())
        checkDistinguishing(witnessFormula, p, qq.head)
      }
      val distinguishingNodeFormulas = for {
        (node, pricedFormulas) <- distinguishingFormulas
          .toSet[((GameNode, Energy), Set[HennessyMilnerLogic.Formula[A]])]
          .groupBy(kv => kv._1._1)
        formulas = for {
          (_, formulasForPrice) <- pricedFormulas
          f <- formulasForPrice
        } yield f
      } yield (node, formulas)

      val bisimilarNodes = for {
        gn <- hmlGame.discovered
        if (gn match { case hmlGame.AttackerObservation(_, qq) => qq.size == 1; case _ => false }) &&
          !hmlGame.attackerVictoryPrices.isDefinedAt(gn)
      } yield (gn, Set[HennessyMilnerLogic.Formula[A]]())

      val distinguishingNodeFormulasExtended = distinguishingNodeFormulas ++ bisimilarNodes

      debugLog(graphvizGameWithFormulas(hmlGame, hmlGame.attackerVictoryPrices.toMap, distinguishingNodeFormulasExtended))

      val bestPreorders: Map[GameNode,List[Spectrum.EquivalenceNotion[ObservationClassFast]]] =
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
      } yield AbstractSpectroscopy.SpectroscopyResultItem[S, A, ObservationClassFast, HennessyMilnerLogic.Formula[A]](p, q, distinctions, preorders)

      AbstractSpectroscopy.SpectroscopyResult[S, A, ObservationClassFast, HennessyMilnerLogic.Formula[A]](spectroResults.toList, spectrum)
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
        hmlGame.attackerVictoryPrices(gn) = Set()
      }

      //debugLog(graphvizGameWithFormulas(hmlGame, hmlGame.attackerVictoryPrices.toMap, Map()))

      val bestPreorders: Map[GameNode,(Set[ObservationClassFast],List[Spectrum.EquivalenceNotion[ObservationClassFast]])] =
        hmlGame.attackerVictoryPrices.toMap.mapValues { energies =>
        // 1 offset in conjunctions between prices and energy metric...
        val fcs = energies.map(e => ObservationClassFast(e(0), e(1) - 1, e(2), e(3)))
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
      } yield AbstractSpectroscopy.SpectroscopyResultItem[S, A, ObservationClassFast, HennessyMilnerLogic.Formula[A]](p, q, distinctions.toList, preorders)

      AbstractSpectroscopy.SpectroscopyResult[S, A, ObservationClassFast, HennessyMilnerLogic.Formula[A]](spectroResults.toList, spectrum)
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
      game: EnergySpectroscopyGame[S, A, L],
      attackerVictoryPrices: Map[GameNode, Set[Energy]],
      formulas: Map[GameNode, Set[HennessyMilnerLogic.Formula[A]]]
  ) = {
    val visualizer = new GameGraphVisualizer(game) {

      def nodeToID(gn: GameNode): String = gn.hashCode().toString()

      def nodeToString(gn: GameNode): String = {
        val priceString = attackerVictoryPrices.getOrElse(gn,Set()).map(_.vector).mkString(" / ")
        val formulaString = formulas.getOrElse(gn,Set()).mkString("\\n").replaceAllLiterally("⟩⊤","⟩")
        (gn match {
          case game.AttackerObservation(p, qq: Set[_]) =>
            val qqString = qq.mkString("{",",","}")
            s"$p, $qqString"
          case game.AttackerClause(p, q) =>
            s"$p, $q"
          case game.DefenderConjunction(p, qq: Set[_]) =>
            val qqString = qq.mkString("{",",","}")
            s"$p, $qqString"
          case _ => ""
        }).replaceAllLiterally(".0", "") +
         (if (priceString != "") s"\\n------\\n$priceString" else "") +
         (if (formulaString != "") s"\\n------\\n$formulaString" else "")
      }

      def edgeToLabel(gn1: GameNode, gn2: GameNode) = ""//gameEdgeToLabel(game, gn1, gn2)
    }

    val attackerWin = attackerVictoryPrices.filter(_._2.nonEmpty).keySet.toSet

    visualizer.outputDot(attackerWin)
  }
}