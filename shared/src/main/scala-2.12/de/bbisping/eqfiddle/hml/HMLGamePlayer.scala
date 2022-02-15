package de.bbisping.eqfiddle.hml

import de.bbisping.eqfiddle.util.Relation
import de.bbisping.eqfiddle.ts.WeakTransitionSystem
import de.bbisping.eqfiddle.util.FixedPoint
import scala.collection.mutable.Queue
import de.bbisping.eqfiddle.game.WinningRegionComputation
import de.bbisping.eqfiddle.game.GameDiscovery
import de.bbisping.eqfiddle.game.SimpleGame
import de.bbisping.eqfiddle.game.AttackGraphBuilder
import de.bbisping.eqfiddle.game.SimpleGame.GameNode
import de.bbisping.eqfiddle.algo.AlgorithmLogging
import de.bbisping.eqfiddle.util.LabeledRelation
import de.bbisping.eqfiddle.util.Partition
import de.bbisping.eqfiddle.game.GameGraphVisualizer

class HMLGamePlayer[S, A, L] (
    val ts: WeakTransitionSystem[S, A, L],
    val nodes: List[S])
  extends AlgorithmLogging[S, A, L] {
  

  abstract sealed class MoveKind
  case class ObservationMove(a: A) extends MoveKind {
    override def toString() = "⟨" + a + "⟩"
  }
  case class ConjunctMove() extends MoveKind {
    override def toString() = "⋀"
  }
  case class NegationMove() extends MoveKind {
    override def toString() = "¬"
  }
  case class DefenderMove() extends MoveKind {
    override def toString() = "*"
  }

  case class AttackerObservation(p: S, qq: Set[S], arrivingMove: MoveKind) extends SimpleGame.AttackerNode
  case class DefenderConjunction(p: S, qqPart: List[Set[S]]) extends SimpleGame.DefenderNode

  class HMLSpectroscopyGame
    extends SimpleGame with GameDiscovery with WinningRegionComputation {

    override def initialNodes: Iterable[GameNode] = Set(
      AttackerObservation(nodes(0), Set(nodes(1)), ConjunctMove()),
      AttackerObservation(nodes(1), Set(nodes(0)), ConjunctMove())
    )

    def successors(gn: GameNode): Iterable[GameNode] = gn match {
      case AttackerObservation(p0, qq0, moveKind) =>
        if (qq0 contains p0) {
          List()
        } else {
          val dn = for {
            (a,pp1) <- ts.post(p0)
            p1 <- pp1
          } yield {
            AttackerObservation(p1,
              qq0.flatMap(ts.post(_, a)),
              ObservationMove(a)
            )
          }
          if (qq0.size == 1 && moveKind.isInstanceOf[ConjunctMove]) {
            // wlog only have negation moves when the defender is focused (which can be forced by the attacker using preceding conjunctions)
            val neg = AttackerObservation(qq0.head, Set(p0), NegationMove())
            dn ++ List(neg)
          } else if (moveKind.isInstanceOf[ObservationMove]) {
            val conjMoves = for {
              parts <- Partition.partitioningListsOfSet(qq0)
              //if parts.length == qq0.size // this is equivalent to the original algorithm's game
              //if parts.length != 1 // drop the trivial partitioning
            } yield {
              DefenderConjunction(p0, parts)
            }
            dn ++ conjMoves
          } else {
            dn
          }
        }
      case DefenderConjunction(p0, qqPart0) =>
        for {
          qq0 <- qqPart0
        } yield {
          AttackerObservation(p0, qq0, ConjunctMove())
        }
    }
  }

  def mergeMoves(node: GameNode, possibleMoves: Iterable[Set[HennessyMilnerLogic.Formula[A]]]): Set[HennessyMilnerLogic.Formula[A]] = {
    val moves: Set[HennessyMilnerLogic.Formula[A]] = node match {
      case DefenderConjunction(_, _) => //if possibleMoves.size != 1 =>
        val productMoves =
          possibleMoves.foldLeft(Seq(Seq[HennessyMilnerLogic.Formula[A]]()))(
            (b, a) => b.flatMap(i => a.map(j => i ++ Seq(j))))
        productMoves.map { mv =>
          val moves = mv.toSet
          HennessyMilnerLogic.And(moves).asInstanceOf[HennessyMilnerLogic.Formula[A]]
        }.toSet
      case AttackerObservation(_, _, ConjunctMove()) =>
        possibleMoves.flatten.toSet
      case AttackerObservation(_, _, NegationMove()) =>
        for {
          f <- possibleMoves.flatten.toSet[HennessyMilnerLogic.Formula[A]]
          //if !f.isInstanceOf[HennessyMilnerLogic.Negate[_]]
        } yield HennessyMilnerLogic.Negate(f)
      case AttackerObservation(_, _, ObservationMove(a)) =>
        possibleMoves.flatten.toSet[HennessyMilnerLogic.Formula[A]].map(HennessyMilnerLogic.Observe(a, _))
    }
    node match {
      case AttackerObservation(_, _, ConjunctMove()) => moves
      case _ => lubMoves(moves, moves)
    }
  }

  def lubMoves(newFormulas: Set[HennessyMilnerLogic.Formula[A]], oldFormulas: Set[HennessyMilnerLogic.Formula[A]]) = {
    val oldFormulaClasses = for {
      f <- oldFormulas
      if f.isInstanceOf[HennessyMilnerLogic.Observe[_]]
    } yield f.getRootClass()
    // if no old formula's class dominates this formula's class...
    for {
      f <- newFormulas
      cl = f.getRootClass()
      if !oldFormulaClasses.exists(clOther => cl.strictlyAbove(clOther))
    } yield {
      f
    }
  }

  def logAttacksAndResult(node: GameNode, attackGraph: Relation[SimpleGame.GameNode], resultFormulas: Set[HennessyMilnerLogic.Formula[A]]) = {
    def gameNodeToTuple(n: SimpleGame.GameNode) = n match {
      case AttackerObservation(p, qq, afterConj) =>
        (Set(p), "A", qq)
      case DefenderConjunction(p, qq) =>
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

  def logDefenseResult(node: GameNode, nodeFormulas: Map[GameNode, Set[HennessyMilnerLogic.Formula[A]]]) = {
   
    val bestPreorders = nodeFormulas.mapValues { ffs =>
      val classes = ffs.flatMap(_.classifyFormula()._2)
      ObservationClass.getStrongestPreorderClass(classes)
    }
    

    val simNodes = for {
      (gn, preorders) <- bestPreorders
      if gn.isInstanceOf[AttackerObservation]
      AttackerObservation(p, qq, _) = gn
      label = preorders.map(_._1).mkString(",")
      q <- qq
    } yield (p, label, q)
    
    val rel = new LabeledRelation(simNodes.toSet)
    val AttackerObservation(p, qq, _) = node

    for {
      q <- qq
      (preorderName, _) <- bestPreorders(node)
    } {
      val msg = p + " preordered to " + q + " by " + preorderName
      logRelation(rel, msg)
    }
  }

  def buildHML(game: HMLSpectroscopyGame, win: Set[GameNode], nodes: Set[GameNode]) = {

    val attackGraphBuilder = new AttackGraphBuilder[Set[HennessyMilnerLogic.Formula[A]]]()

    val attackGraph = attackGraphBuilder.buildAttackGraph(game, win, nodes)

    val accumulatedPrices = attackGraphBuilder.accumulatePrices(
      graph = attackGraph,
      priceCons = (_, _, ff) => ff,
      pricePick = mergeMoves _,
      supPrice = Set(),
      nodes = nodes
    )

    val minPrices = accumulatedPrices.mapValues(HennessyMilnerLogic.getLeastDistinguishing(_))

    if (AlgorithmLogging.loggingActive) {
      nodes.foreach { n => logAttacksAndResult(n, attackGraph, minPrices(n)) }
      nodes.foreach { n => logDefenseResult(n, minPrices)}
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

  def graphvizGameWithFormulas(game: HMLSpectroscopyGame, win: Set[GameNode], formulas: Map[GameNode, Set[HennessyMilnerLogic.Formula[A]]]) = {
    val visualizer = new GameGraphVisualizer(game) {

      def nodeToID(gn: GameNode): String = gn.hashCode().toString()

      def nodeToString(gn: GameNode): String = gn match {
        case AttackerObservation(p, qq: Set[_], kind) =>
          val qqString = qq.mkString("{",",","}")
          val formulaString = formulas.getOrElse(gn,Set()).mkString("\\n").replaceAllLiterally("⟩⊤","⟩")
          val label = s"$p, $qqString, $kind" +
            (if (formulaString != "{}") s"\\n------\\n$formulaString" else "")
          label.replaceAllLiterally(".0", "")
        case DefenderConjunction(p, qqPart: List[Set[_]]) =>
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

    val hmlGame = new HMLSpectroscopyGame()

    debugLog("HML spectroscopy game size: " + hmlGame.discovered.size)

    val attackerWin = hmlGame.computeWinningRegion()
    val aLR = AttackerObservation(nodes(0), Set(nodes(1)), ConjunctMove())
    val aRL = AttackerObservation(nodes(1), Set(nodes(0)), ConjunctMove())

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
        if gn.isInstanceOf[AttackerObservation] && !attackerWin(gn)
        AttackerObservation(p, qq, _) = gn
        q <- qq
      } yield (p, "", q)

      val rel = new LabeledRelation(simNodes.toSet)
      logRelation(rel, nodes(0) + " and " + nodes(1) + " are bisimulation equivalent.")

      debugLog(graphvizGameWithFormulas(hmlGame, attackerWin, Map().withDefaultValue(Set())))
    }

    true
  }
}