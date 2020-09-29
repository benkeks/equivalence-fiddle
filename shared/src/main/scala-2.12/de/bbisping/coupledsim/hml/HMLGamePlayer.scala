package de.bbisping.coupledsim.algo.cs

import de.bbisping.coupledsim.util.Relation
import de.bbisping.coupledsim.ts.WeakTransitionSystem
import de.bbisping.coupledsim.util.FixedPoint
import scala.collection.mutable.Queue
import de.bbisping.coupledsim.game.WinningRegionComputation
import de.bbisping.coupledsim.game.GameDiscovery
import de.bbisping.coupledsim.game.SimpleGame
import de.bbisping.coupledsim.hml.HennessyMilnerLogic
import de.bbisping.coupledsim.game.AttackTreeBuilder
import de.bbisping.coupledsim.game.SimpleGame.GameNode


class HMLGamePlayer[S, A, L] (
    val ts: WeakTransitionSystem[S, A, L],
    val nodes: List[S]) {
  

  abstract sealed class MoveKind
  case class ObservationMove(a: A) extends MoveKind
  case class ConjunctMove() extends MoveKind
  case class NegationMove() extends MoveKind
  case class DefenderMove() extends MoveKind

  val recordedMoveEdges = collection.mutable.Map[(GameNode, GameNode), MoveKind]()

  case class AttackerObservation(p: S, qq: Set[S]) extends SimpleGame.AttackerNode
  case class DefenderConjunction(p: S, qq: Set[S]) extends SimpleGame.DefenderNode
  case class DefenderNegation(p: S, qq: Set[S]) extends SimpleGame.DefenderNode

  class HMLGame
    extends SimpleGame with GameDiscovery with WinningRegionComputation {

    override def initialNodes: Iterable[GameNode] = Set(
      AttackerObservation(nodes(0), Set(nodes(1))),
      AttackerObservation(nodes(1), Set(nodes(0)))
    )

    def successors(gn: GameNode): Iterable[GameNode] = gn match {
      case AttackerObservation(p0, qq0) =>
        val dn = for {
          (a,pp1) <- ts.post(p0)
          p1 <- pp1
          val next = AttackerObservation(p1, qq0.flatMap(ts.post(_, a)))
        } yield {
          recordedMoveEdges((gn, next)) = ObservationMove(a)
          next
        }
        val conj = DefenderConjunction(p0, qq0)
        recordedMoveEdges((gn, conj)) = ConjunctMove()
        if (qq0.nonEmpty) {
          // only add negation moves if the defender hasn't trivially lost already (in order to have unique finishing moves with Conj for the attacker)
          val neg = DefenderNegation(p0, qq0)
          recordedMoveEdges((gn, neg)) = NegationMove()
          dn ++ List(conj, neg)
        } else {
          dn ++ List(conj)
        }
      case DefenderConjunction(p0, qq0) =>
        for {
          q0 <- qq0
          obs = AttackerObservation(p0, Set(q0))
        } yield {
          recordedMoveEdges((gn, obs)) = DefenderMove()
          obs
        }
      case DefenderNegation(p0, qq0) =>
        for {
          q0 <- qq0
          obs = AttackerObservation(q0, Set(p0))
        } yield {
          recordedMoveEdges((gn, obs)) = DefenderMove()
          obs
        }
    }
  }

  def buildHML(game: HMLGame, win: Set[GameNode], node: GameNode) = {

    //val localEdges = recordedMoveEdges.toMap
    //def pickMin(edges: Iterable[MoveKind]) = edges.head

    val attackTreeBuilder = new AttackTreeBuilder[Set[HennessyMilnerLogic.Formula[A]]]()

    // (TODO: this shouldnt need to be a tree at this point...)
    val attackTree = attackTreeBuilder.buildAttackTree(game, win, node)

    println("Attack Tree: " + attackTree)

    val defenderDefeats = attackTree.rhs.collect {
      case emptyDef @ DefenderConjunction(_, conjs) if conjs.isEmpty => emptyDef.asInstanceOf[GameNode]
    }

    println("Defender Defeats: " + defenderDefeats.mkString("{", ",", "}"))

    def moveToHML(n1: GameNode, n2: GameNode, ff: Set[HennessyMilnerLogic.Formula[A]]): Set[HennessyMilnerLogic.Formula[A]] = {
      val kind = recordedMoveEdges(n1, n2)

      kind match {
        case ConjunctMove() =>
          ff//HennessyMilnerLogic.And(ff.toList)
        case NegationMove() =>
          ff//TODO: Is this correct? 2020-09-28
        case ObservationMove(a) =>
          ff.map(HennessyMilnerLogic.Observe(a, _))
        case DefenderMove() =>
          ff
      }
    }
    
    def mergeMoves(node: GameNode, possibleMoves: Iterable[Set[HennessyMilnerLogic.Formula[A]]]): Set[HennessyMilnerLogic.Formula[A]] = node match {
      case DefenderConjunction(_, _) =>
        val productMoves =
          possibleMoves.foldLeft(Seq(Seq[HennessyMilnerLogic.Formula[A]]()))(
            (b, a) => b.flatMap(i => a.map(j => i ++ Seq(j))))
        productMoves.map(mv => HennessyMilnerLogic.And(mv.toList).asInstanceOf[HennessyMilnerLogic.Formula[A]]).toSet
      
      case DefenderNegation(_, _) =>
        val productMoves =
          possibleMoves.foldLeft(Seq(Seq[HennessyMilnerLogic.Formula[A]]()))(
            (b, a) => b.flatMap(i => a.map(j => i ++ Seq(j))))
        
        productMoves.map { mv =>
          if (mv.size == 1) {
            HennessyMilnerLogic.Negate(mv.head)
          } else {
            HennessyMilnerLogic.And(mv.toList.map(HennessyMilnerLogic.Negate(_))).asInstanceOf[HennessyMilnerLogic.Formula[A]]
          }
        }.toSet

      case _ =>
        possibleMoves.flatten.toSet
    }

    val accumulatedPrices = attackTreeBuilder.accumulatePrices(
      tree = attackTree,
      priceCons = moveToHML _,
      pricePick = mergeMoves _,
      finishingPrice = Set(HennessyMilnerLogic.And(List())),
      supPrice = Set(),
      node = node,
      targetRegion = defenderDefeats
    )

    accumulatedPrices(node)
  }

  def compute() = {

    val hmlGame = new HMLGame()

    println("hml game size: " + hmlGame.discovered.size)

    val attackerWin = hmlGame.computeWinningRegion()

    val aLR = AttackerObservation(nodes(0), Set(nodes(1)))
    val aRL = AttackerObservation(nodes(1), Set(nodes(0)))

    println("left simBy right:" + !attackerWin.contains(aLR))

    if (attackerWin.contains(aLR)) {
      val formulas = buildHML(hmlGame, attackerWin, aLR)
      formulas.foreach {f => println("Distinguished under " + f.classifyFormula() + " preorder by " + f.toString())}
    }

    println("right simBy left:" + !attackerWin.contains(aRL))
    if (attackerWin.contains(aRL)) {
      val formulas = buildHML(hmlGame, attackerWin, aRL)
      formulas.foreach {f => println("Distinguished under " + f.classifyFormula() + " preorder by " + f.toString())}
    }

    true
  }
}