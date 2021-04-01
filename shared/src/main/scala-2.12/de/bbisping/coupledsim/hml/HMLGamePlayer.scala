package de.bbisping.coupledsim.hml

import de.bbisping.coupledsim.util.Relation
import de.bbisping.coupledsim.ts.WeakTransitionSystem
import de.bbisping.coupledsim.util.FixedPoint
import scala.collection.mutable.Queue
import de.bbisping.coupledsim.game.WinningRegionComputation
import de.bbisping.coupledsim.game.GameDiscovery
import de.bbisping.coupledsim.game.SimpleGame
import de.bbisping.coupledsim.game.AttackGraphBuilder
import de.bbisping.coupledsim.game.SimpleGame.GameNode
import de.bbisping.coupledsim.algo.AlgorithmLogging
import de.bbisping.coupledsim.util.LabeledRelation

class HMLGamePlayer[S, A, L] (
    val ts: WeakTransitionSystem[S, A, L],
    val nodes: List[S])
  extends AlgorithmLogging[S, A, L] {
  

  abstract sealed class MoveKind
  case class ObservationMove(a: A) extends MoveKind {
    override def toString() = "⟨" + a + "⟩"
  }

  // case class PassingMove() extends MoveKind {
  //   override def toString() = "⟨ϵ⟩"
  // }
  case class ImmediacyMove() extends MoveKind {
    override def toString() = "!"
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

  val recordedMoveEdges = collection.mutable.Map[(GameNode, GameNode), MoveKind]()

  case class AttackerObservation(p: S, qq: Set[S], immediacy: Boolean = false) extends SimpleGame.AttackerNode 
  case class DefenderConjunction(p: S, qq: Set[S]) extends SimpleGame.DefenderNode

  class HMLSpectroscopyGame
    extends SimpleGame with GameDiscovery with WinningRegionComputation {

    override def initialNodes: Iterable[GameNode] = Set(
      AttackerObservation(nodes(0), Set(nodes(1))),
      AttackerObservation(nodes(1), Set(nodes(0)))
    )

    def successors(gn: GameNode): Iterable[GameNode] = gn match {
      case AttackerObservation(p0, qq0, immediacy) =>
        val dn = for {
          (a,pp1) <- ts.post(p0)
          if !(immediacy && ts.silentActions(a)) // <- allow attacker to move down tau steps only in non immediate mode, for now
          p1 <- pp1
          next = if (immediacy) {
            AttackerObservation(p1, qq0.flatMap(ts.post(_, a)), immediacy = false)
          } else {
            AttackerObservation(p1, qq0.flatMap(ts.weakPostDelay(_, a)), immediacy = false)
          }
          
        } yield {
          recordedMoveEdges((gn, next)) = ObservationMove(a)
          next
        }

        //TODO only if no immediacy!
        val immediate = AttackerObservation(p0, qq0, immediacy = true)
        recordedMoveEdges((gn, immediate)) = ImmediacyMove()
        
        val conj = if (immediacy) {
          DefenderConjunction(p0, qq0)
        } else {
          DefenderConjunction(p0, qq0.flatMap(ts.silentReachable(_)))
        }
        recordedMoveEdges((gn, conj)) = ConjunctMove()

        (if (qq0.size == 1) {
          // wlog only have negation moves when the defender is focused (which can be forced by the attacker using a preceding conjunction)
          val neg = AttackerObservation(qq0.head, Set(p0))
          recordedMoveEdges((gn, neg)) = NegationMove()
          List(neg)
        } else {
          List()
          // immediate conjunct moves only make sense if the defender is spread
        }) ++ dn ++ List(immediate, conj)
      case DefenderConjunction(p0, qq0) =>
        for {
          q0 <- qq0
          obs = AttackerObservation(p0, Set(q0))
        } yield {
          recordedMoveEdges((gn, obs)) = DefenderMove()
          obs
        }
    }
  }

  def moveToHML(n1: GameNode, n2: GameNode, ff: Set[HennessyMilnerLogic.Formula[A]]): Set[HennessyMilnerLogic.Formula[A]] = {
    val kind = recordedMoveEdges(n1, n2)

    kind match {
      case ConjunctMove() =>
        ff
      case NegationMove() =>
        ff.map(HennessyMilnerLogic.Negate(_))
      case ObservationMove(a) =>
        ff.map(HennessyMilnerLogic.Observe(a, _))
      case ImmediacyMove() =>
        ff.map(HennessyMilnerLogic.Immediate(_))
      case DefenderMove() =>
        ff
    }
  }
  
  def mergeMoves(node: GameNode, possibleMoves: Iterable[Set[HennessyMilnerLogic.Formula[A]]]): Set[HennessyMilnerLogic.Formula[A]] = node match {
    case DefenderConjunction(_, _) if possibleMoves.size != 1 =>
      val productMoves =
        possibleMoves.foldLeft(Seq(Seq[HennessyMilnerLogic.Formula[A]]()))(
          (b, a) => b.flatMap(i => a.map(j => i ++ Seq(j))))
      productMoves.map { mv =>
        val moves = mv.toSet
        if (moves.size == 1) {
          moves.head
        } else {
          HennessyMilnerLogic.And(moves).asInstanceOf[HennessyMilnerLogic.Formula[A]]
        }
      }.toSet
    case _ =>
      val possibleFormulas = possibleMoves.flatten.toSet
      if (possibleFormulas.size > 1) {
        lubMoves(possibleFormulas, possibleFormulas)
      } else {
        possibleFormulas
      }
  }

  def lubMoves(newFormulas: Set[HennessyMilnerLogic.Formula[A]], oldFormulas: Set[HennessyMilnerLogic.Formula[A]]) = {

    val formulaClasses = for {
      f <- oldFormulas
    } yield f.getRootClass()

    // if no old formula's class dominates this formula's class...
    for {
      f <- newFormulas
      cl = f.getRootClass()
      // privilege for failures and impossible futures and weak formulas
      if (cl.height <= 1 && cl.negationLevels <= 1) || 
        (cl.negationLevels == 1 && cl.maxNegationHeight == cl.height) ||
        !formulaClasses.exists(clOther => cl.above(clOther) && cl != clOther)
    } yield {
      f
    }
  }

  def logAttacksAndResult(node: GameNode, attackGraph: Relation[SimpleGame.GameNode], resultFormulas: Set[HennessyMilnerLogic.Formula[A]]) = {
    def gameNodeToTuple(n: SimpleGame.GameNode) = n match {
      case AttackerObservation(p, qq, false) => 
        (Set(p), "A", qq)
      case AttackerObservation(p, qq, true) => 
        (Set(p), "A!", qq)
      case DefenderConjunction(p, qq) => 
        (Set(p), "D", qq)
    }
    
    val gameRel: Set[((Set[S], String, Set[S]), String, (Set[S], String, Set[S]))] = for {
      (n1, n2) <- attackGraph.tupleSet
    } yield (gameNodeToTuple(n1), recordedMoveEdges(n1, n2).toString(), gameNodeToTuple(n2))
    
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
      AttackerObservation(p, qq, _) = gn // TODO non?-immediate
      label = preorders.map(_._1).mkString(",")
      q <- qq
    } yield (p, label, q)
    
    val rel = new LabeledRelation(simNodes.toSet)
    val AttackerObservation(p, qq, _) = node // TODO non?-immediate

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
      priceCons = moveToHML _,
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
    // val hmlInterpreter = new HMLInterpreter(ts)
    // val check = hmlInterpreter.isTrueAt(formula, List(p, q))
    // if (!check(p) || check(q)) {
    //   System.err.println("Formula " + formula.toString() + " is no sound distinguishing formula! " + check)
    // }
  }

  def compute() = {

    val hmlGame = new HMLSpectroscopyGame()

    debugLog("HML spectroscopy game size: " + hmlGame.discovered.size)

    val attackerWin = hmlGame.computeWinningRegion()
    val aLR = AttackerObservation(nodes(0), Set(nodes(1)))
    val aRL = AttackerObservation(nodes(1), Set(nodes(0)))

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
    } else {

      val simNodes = for {
        gn <- hmlGame.discovered
        if gn.isInstanceOf[AttackerObservation] && !attackerWin(gn)
        AttackerObservation(p, qq, _) = gn // TODO non?-immediate
        q <- qq
      } yield (p, "", q)
    
      val rel = new LabeledRelation(simNodes.toSet)

      logRelation(rel, nodes(0) + " and " + nodes(1) + " are bisimulation equivalent.")
    }

    true
  }
}