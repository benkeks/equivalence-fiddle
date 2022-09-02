package de.bbisping.eqfiddle.spectroscopy

import de.bbisping.eqfiddle.ts.WeakTransitionSystem
import de.bbisping.eqfiddle.algo.AlgorithmLogging
import de.bbisping.eqfiddle.hml.ObservationClassFast
import de.bbisping.eqfiddle.hml.Spectrum
import de.bbisping.eqfiddle.game.SimpleGame
import de.bbisping.eqfiddle.game.SimpleGame.GameNode
import de.bbisping.eqfiddle.hml.HennessyMilnerLogic
import de.bbisping.eqfiddle.hml.HMLInterpreter
import de.bbisping.eqfiddle.game.GameGraphVisualizer

class FastSpectroscopy[S, A, L] (
    ts: WeakTransitionSystem[S, A, L],
    nodes: List[S])
  extends AlgorithmLogging[S] {

  val spectrum = ObservationClassFast.LTBTS

  val distinguishingFormulas = collection.mutable.Map[(GameNode, ObservationClassFast), Set[HennessyMilnerLogic.Formula[A]]]()

  def buildHMLWitness(game: FastSpectroscopyGame[S, A, L], node: GameNode, price: ObservationClassFast): Set[HennessyMilnerLogic.Formula[A]]
    = distinguishingFormulas.getOrElseUpdate((node, price), {
    //debugLog(s"exploring: $node, $price" )
    node match {
      case game.AttackerObservation(p0, qq0, false) if qq0.isEmpty =>
        Set(HennessyMilnerLogic.True)
      case game.AttackerObservation(p0, qq0, postConj0) =>
        val successorFormulas =
          for {
            s <- game.successors(node)
            sP = game.attackerVictoryPrices(s)
            //currP <- currentVictoryPrices
            if sP.exists(_ < price) // ensure descent
          } yield s match {
            case game.AttackerObservation(p1, qq1, postConj1) if !postConj0 && sP.exists(_.observationHeight < price.observationHeight) =>
              val possibleRestoredActions = for {
                (a, pp1) <- ts.post(p0)
                if pp1 contains p1
                if qq1 == ts.post(qq0,a)
              } yield a
              val newPriceBound = ObservationClassFast(price.observationHeight - 1, price.conjunctionLevels, price.maxPositiveConjunctHeight, price.maxNegativeConjunctHeight)
              for {
                a <- possibleRestoredActions.headOption.toSet[A] // just take first option
                postForm <- buildHMLWitness(game, s, newPriceBound)
              } yield HennessyMilnerLogic.Observe(a, postForm)
            case game.AttackerObservation(p1, qq1, postConj1) if postConj0 =>
              {
                if (Set(p1) == qq0 && Set(p0) == qq1 && sP.exists(price.maxNegativeConjunctHeight >= _.observationHeight)) { // side swap
                  val newPrice = ObservationClassFast(Math.max(price.observationHeight, price.maxNegativeConjunctHeight),
                    price.conjunctionLevels, Math.max(price.observationHeight, price.maxNegativeConjunctHeight), price.maxNegativeConjunctHeight)
                  for {
                    postForm <- buildHMLWitness(game, s, newPrice)
                  } yield HennessyMilnerLogic.Negate(postForm)
                } else {
                  Set[HennessyMilnerLogic.Formula[A]]()
                }
              } ++ {
                val newPrice = ObservationClassFast(Math.max(price.observationHeight, price.maxPositiveConjunctHeight),
                  price.conjunctionLevels, price.maxPositiveConjunctHeight, price.maxNegativeConjunctHeight)
                if (p0 == p1 && qq0 == qq1 && sP.exists(price.maxPositiveConjunctHeight >= _.observationHeight)) { // no side swap (= positive branch)
                  buildHMLWitness(game, s, price)
                } else {
                  Set[HennessyMilnerLogic.Formula[A]]()
                }
              }
            case game.DefenderConjunction(p1, qq1) =>
              val newPriceBound = ObservationClassFast(price.observationHeight, price.conjunctionLevels - 1, price.maxPositiveConjunctHeight, price.maxNegativeConjunctHeight)
              buildHMLWitness(game, s, newPriceBound)
            case _ => Set()
          }
        successorFormulas.toSet.flatten
      case defNode: game.DefenderConjunction =>
        val possibleMoves = for {
          s <- game.successors(node)
          sP = game.attackerVictoryPrices(s)
        } yield if (sP.exists(_ <= price)) {
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

  def compute(): AbstractSpectroscopy.SpectroscopyResult[S, A, ObservationClassFast, HennessyMilnerLogic.Formula[A]] = {

    val hmlGame = new FastSpectroscopyGame(ts)

    val init = List(hmlGame.AttackerObservation(nodes(0), Set(nodes(1))), hmlGame.AttackerObservation(nodes(1), Set(nodes(0))))
    def instantAttackerWin(gn: GameNode) = gn match {case hmlGame.DefenderConjunction(_, qq) if qq.isEmpty => List(ObservationClassFast()); case _ => List()}

    hmlGame.populateGame(
      init,
      hmlGame.computeSuccessors(_),
      instantAttackerWin(_))

    debugLog("HML spectroscopy game size: " + hmlGame.discovered.size)

    for {
      bestPrice <- hmlGame.attackerVictoryPrices(init.head)
      witnessFormula <- buildHMLWitness(hmlGame, init.head, bestPrice)
    } {
      debugLog("Distinguished under " + spectrum.classifyFormula(witnessFormula) + " preorder by " + witnessFormula.toString())
      checkDistinguishing(witnessFormula, nodes(0), nodes(1))
    }

    val distinguishingNodeFormulas = for {
      (node, pricedFormulas) <- distinguishingFormulas
        .toSet[((GameNode, ObservationClassFast), Set[HennessyMilnerLogic.Formula[A]])]
        .groupBy(kv => kv._1._1)
      formulas = for {
        (_, formulasForPrice) <- pricedFormulas
        f <- formulasForPrice
      } yield f
    } yield (node, formulas)

    debugLog(graphvizGameWithFormulas(hmlGame, hmlGame.attackerVictoryPrices.toMap, distinguishingNodeFormulas))

    val bestPreorders: Map[GameNode,List[Spectrum.EquivalenceNotion[ObservationClassFast]]] =
      distinguishingNodeFormulas.mapValues { ffs =>
      val classes = ffs.flatMap(spectrum.classifyFormula(_)._2)
      spectrum.getStrongestPreorderClass(classes)
    }

    val spectroResults = for {
      gn <- hmlGame.discovered
      if gn.isInstanceOf[hmlGame.AttackerObservation]
      hmlGame.AttackerObservation(p, qq, kind) = gn
      if qq.size == 1
      q <- qq
      preorders <- bestPreorders.get(gn)
      distinctionFormulas = distinguishingNodeFormulas(gn)
      distinctions = for {
        f <- distinctionFormulas.toList
        (price, eqs) = spectrum.classifyFormula(f)
      } yield (f, price, eqs)
    } yield AbstractSpectroscopy.SpectroscopyResultItem[S, A, ObservationClassFast, HennessyMilnerLogic.Formula[A]](p, q, distinctions, preorders)

    AbstractSpectroscopy.SpectroscopyResult[S, A, ObservationClassFast, HennessyMilnerLogic.Formula[A]](spectroResults.toList, spectrum)
  }

  def checkDistinguishing(formula: HennessyMilnerLogic.Formula[A], p: S, q: S) = {
    val hmlInterpreter = new HMLInterpreter(ts)
    val check = hmlInterpreter.isTrueAt(formula, List(p, q))
    if (!check(p) || check(q)) {
      System.err.println("Formula " + formula.toString() + " is no sound distinguishing formula! " + check)
    }
  }

  def graphvizGameWithFormulas(
      game: FastSpectroscopyGame[S, A, L],
      attackerVictoryPrices: Map[GameNode, Set[ObservationClassFast]],
      formulas: Map[GameNode, Set[HennessyMilnerLogic.Formula[A]]]
  ) = {
    val visualizer = new GameGraphVisualizer(game) {

      def nodeToID(gn: GameNode): String = gn.hashCode().toString()

      def nodeToString(gn: GameNode): String = {
        val priceString = attackerVictoryPrices.getOrElse(gn,Set()).map(_.toTuple).mkString(" / ")
        val formulaString = formulas.getOrElse(gn,Set()).mkString("\\n").replaceAllLiterally("⟩⊤","⟩")
        (gn match {
          case game.AttackerObservation(p, qq: Set[_], kind) =>
            val qqString = qq.mkString("{",",","}")
            s"$p, $qqString, $kind"
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