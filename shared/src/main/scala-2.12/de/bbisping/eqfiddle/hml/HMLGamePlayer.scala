package de.bbisping.eqfiddle.hml

import de.bbisping.eqfiddle.util.Relation
import de.bbisping.eqfiddle.ts.WeakTransitionSystem
import de.bbisping.eqfiddle.util.FixedPoint
import de.bbisping.eqfiddle.game.WinningRegionComputation
import de.bbisping.eqfiddle.game.AttackGraphBuilder
import de.bbisping.eqfiddle.game.SimpleGame.GameNode
import de.bbisping.eqfiddle.algo.AlgorithmLogging
import de.bbisping.eqfiddle.util.LabeledRelation
import de.bbisping.eqfiddle.game.GameGraphVisualizer

class HMLGamePlayer[S, A, L] (
    val ts: WeakTransitionSystem[S, A, L],
    val nodes: List[S])
  extends AlgorithmLogging[S, A, L] {

  def buildStrategyFormulas(game: SpectroscopyGame[S, A, L])(node: GameNode, possibleMoves: Iterable[Set[HennessyMilnerLogic.Formula[A]]]): Set[HennessyMilnerLogic.Formula[A]] = {
    val moves: Set[HennessyMilnerLogic.Formula[A]] = node match {
      case game.DefenderConjunction(_, _) =>
        val productMoves =
          possibleMoves.foldLeft(Seq(Seq[HennessyMilnerLogic.Formula[A]]()))(
            (b, a) => b.flatMap(i => a.map(j => i ++ Seq(j))))
        productMoves.map { mv =>
          val moves = mv.toSet
          HennessyMilnerLogic.And(moves).asInstanceOf[HennessyMilnerLogic.Formula[A]]
        }.toSet
      case game.AttackerObservation(_, _, game.ConjunctMove) =>
        possibleMoves.flatten.toSet
      case game.AttackerObservation(_, _, game.NegationMove) =>
        possibleMoves.flatten.toSet[HennessyMilnerLogic.Formula[A]].map(HennessyMilnerLogic.Negate(_))
      case game.AttackerObservation(_, _, game.ObservationMove(a)) =>
        possibleMoves.flatten.toSet[HennessyMilnerLogic.Formula[A]].map(HennessyMilnerLogic.Observe[A](a, _))
    }
    node match {
      case game.AttackerObservation(_, _, game.ConjunctMove) => moves
      case _ => pruneDominated(moves)
    }
  }

  def pruneDominated(oldFormulas: Set[HennessyMilnerLogic.Formula[A]]) = {
    val oldFormulaClasses = for {
      f <- oldFormulas
      if f.isInstanceOf[HennessyMilnerLogic.Observe[_]]
    } yield f.getRootClass()
    // if no old formula's class dominates this formula's class...
    for {
      f <- oldFormulas
      cl = f.getRootClass()
      if !oldFormulaClasses.exists(clOther => cl.strictlyAbove(clOther))
    } yield {
      f
    }
  }

  def logAttacksAndResult(game: SpectroscopyGame[S, A, L], node: GameNode, attackGraph: Relation[GameNode], resultFormulas: Set[HennessyMilnerLogic.Formula[A]]) = {
    def gameNodeToTuple(n: GameNode) = n match {
      case game.AttackerObservation(p, qq, afterConj) =>
        (Set(p), "A", qq)
      case game.DefenderConjunction(p, qq) =>
        //TODO: This display does not work anymore with the paritioning approach!
        (Set(p), "D", qq.flatten.toSet)
    }
    
    val gameRel: Set[((Set[S], String, Set[S]), String, (Set[S], String, Set[S]))] = for {
      (n1, n2) <- attackGraph.tupleSet
    } yield (gameNodeToTuple(n1), "", gameNodeToTuple(n2))
    
    val msg = for {
      f <- resultFormulas
      s = gameNodeToTuple(node)
      p <- s._1.headOption
      q <- s._3.headOption
    } yield {
      p + " distinguished from " + q + " under " + f.classifyNicely() + " preorder by " + f.toString()
    }

    logRichRelation(new LabeledRelation(gameRel), msg.mkString("<br>\n"))

  }

  def logDefenseResult(game: SpectroscopyGame[S, A, L], node: GameNode, nodeFormulas: Map[GameNode, Set[HennessyMilnerLogic.Formula[A]]]) = {
   
    val bestPreorders = nodeFormulas.mapValues { ffs =>
      val classes = ffs.flatMap(_.classifyFormula()._2)
      ObservationClass.getStrongestPreorderClass(classes)
    }

    val simNodes = for {
      (gn, preorders) <- bestPreorders
      if gn.isInstanceOf[game.AttackerObservation]
      game.AttackerObservation(p, qq, _) = gn
      label = preorders.map(_._1).mkString(",")
      q <- qq
    } yield (p, label, q)
    
    val rel = new LabeledRelation(simNodes.toSet)
    val game.AttackerObservation(p, qq, _) = node

    for {
      q <- qq
      (preorderName, _) <- bestPreorders(node)
    } {
      val msg = p + " preordered to " + q + " by " + preorderName
      logRelation(rel, msg)
    }
  }

  def buildHML(game: SpectroscopyGame[S, A, L], win: Set[GameNode], nodes: Set[GameNode]) = {

    val attackGraphBuilder = new AttackGraphBuilder[Set[HennessyMilnerLogic.Formula[A]]]()

    val attackGraph = attackGraphBuilder.buildAttackGraph(game, win, nodes)

    val accumulatedPrices = attackGraphBuilder.accumulateNodePrices(
      graph = attackGraph,
      pricePick = buildStrategyFormulas(game) _,
      supPrice = Set(),
      nodes = nodes
    )

    val minPrices = accumulatedPrices.mapValues(HennessyMilnerLogic.getLeastDistinguishing(_))

    if (AlgorithmLogging.loggingActive) {
      nodes.foreach { n => logAttacksAndResult(game, n, attackGraph, minPrices(n)) }
      nodes.foreach { n => logDefenseResult(game, n, minPrices)}
    }

    minPrices
  }

  def checkDistinguishing(formula: HennessyMilnerLogic.Formula[A], p: S, q: S) = {
    val hmlInterpreter = new HMLInterpreter(ts)
    val check = hmlInterpreter.isTrueAt(formula, List(p, q))
    if (!check(p) || check(q)) {
      System.err.println("Formula " + formula.toString() + " is no sound distinguishing formula! " + check)
    }
  }

  def graphvizGameWithFormulas(game: SpectroscopyGame[S, A, L], win: Set[GameNode], formulas: Map[GameNode, Set[HennessyMilnerLogic.Formula[A]]]) = {
    val visualizer = new GameGraphVisualizer(game) {

      def nodeToID(gn: GameNode): String = gn.hashCode().toString()

      def nodeToString(gn: GameNode): String = gn match {
        case game.AttackerObservation(p, qq: Set[_], kind) =>
          val qqString = qq.mkString("{",",","}")
          val formulaString = formulas.getOrElse(gn,Set()).mkString("\\n").replaceAllLiterally("⟩⊤","⟩")
          val label = s"$p, $qqString, $kind" +
            (if (formulaString != "{}") s"\\n------\\n$formulaString" else "")
          label.replaceAllLiterally(".0", "")
        case game.DefenderConjunction(p, qqPart: List[Set[_]]) =>
          val qqString = qqPart.map(_.mkString("{",",","}")).mkString("/")
          (s"$p, $qqString").replaceAllLiterally(".0", "")
        case _ => ""
      }

      def edgeToLabel(gn1: GameNode, gn2: GameNode) = {
        ""
      }
      
    }

    visualizer.outputDot(win)
  }

  def compute() = {

    val hmlGame = new SpectroscopyGame(ts, List((nodes(0), Set(nodes(1))), (nodes(1), Set(nodes(0)))))

    debugLog("HML spectroscopy game size: " + hmlGame.discovered.size)

    val attackerWin = hmlGame.computeWinningRegion()
    val aLR = hmlGame.AttackerObservation(nodes(0), Set(nodes(1)), hmlGame.ConjunctMove)
    val aRL = hmlGame.AttackerObservation(nodes(1), Set(nodes(0)), hmlGame.ConjunctMove)

    if (attackerWin.contains(aLR) || attackerWin.contains(aRL)) {
      val minFormulas = buildHML(hmlGame, attackerWin, Set(aLR, aRL))

      if (attackerWin.contains(aLR)) {
        minFormulas(aLR).foreach { f =>
          debugLog("Distinguished under " + f.classifyFormula() + " preorder by " + f.toString())
          checkDistinguishing(f, nodes(0), nodes(1))
        }
      }

      if (attackerWin.contains(aRL)) {
        minFormulas(aRL).foreach { f =>
          debugLog("Distinguished under " + f.classifyFormula() + " preorder by " + f.toString())
          checkDistinguishing(f, nodes(1), nodes(0))
        }
      }
      debugLog(graphvizGameWithFormulas(hmlGame, attackerWin, minFormulas))
    } else {

      val simNodes = for {
        gn <- hmlGame.discovered
        if gn.isInstanceOf[hmlGame.AttackerObservation] && !attackerWin(gn)
        hmlGame.AttackerObservation(p, qq, _) = gn
        q <- qq
      } yield (p, "", q)

      val rel = new LabeledRelation(simNodes.toSet)
      logRelation(rel, nodes(0) + " and " + nodes(1) + " are bisimulation equivalent.")

      debugLog(graphvizGameWithFormulas(hmlGame, attackerWin, Map().withDefaultValue(Set())))
    }

    true
  }
}