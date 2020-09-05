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
  case class DefenderMove() extends MoveKind

  val recordedMoveEdges = collection.mutable.Map[(GameNode, GameNode), MoveKind]()

  case class AttackerObservation(p: S, qq: Set[S]) extends SimpleGame.AttackerNode
  case class DefenderConjunction(p: S, qq: Set[S]) extends SimpleGame.DefenderNode

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
        dn ++ List(conj)
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

  def buildHML(game: HMLGame, win: Set[GameNode], node: GameNode) = {

    //val localEdges = recordedMoveEdges.toMap
    //def pickMin(edges: Iterable[MoveKind]) = edges.head

    val attackTreeBuilder = new AttackTreeBuilder[Set[HennessyMilnerLogic.Formula[A]]]()

    // (TODO: this shouldnt need to be a tree at this point...)
    val attackTree = attackTreeBuilder.buildAttackTree(game, win, node)

    println("Attack Tree:" + attackTree)

    val defenderDefeats = attackTree.rhs.collect {
      case emptyDef @ DefenderConjunction(_, conjs) if conjs.isEmpty => emptyDef.asInstanceOf[GameNode]
    }

    println(defenderDefeats)

    def moveToHML(n1: GameNode, n2: GameNode, ff: Set[HennessyMilnerLogic.Formula[A]]): Set[HennessyMilnerLogic.Formula[A]] = {
      val kind = recordedMoveEdges(n1, n2)

      kind match {
        case ConjunctMove() =>
          ff//HennessyMilnerLogic.And(ff.toList)
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

    /*
    attackTree,
      ((a,b) => a), pickMin _, ConjunctMove(), ConjunctMove(), node, defenderDefeats.toSet)


    println(defenderDefeats)

    val accumulatedPrices = attackTreeBuilder.accumulatePrices(attackTree, ((a,b) => a), pickMin _, ConjunctMove(), ConjunctMove(), node, defenderDefeats.toSet)

    println(accumulatedPrices)

    val bestAttacks = attackTree.determinize { succs =>
      succs.reduceLeft[(MoveKind, GameNode)] { case (p1n1@(p1, n1), p2n2@(p2, n2)) =>
        //if (accumulatedPrices(n1) <= accumulatedPrices(n2)) p1n1 else p2n2
        p1n1
      }
    }

    println(bestAttacks)
    */

    // def attackToHML(node: GameNode): HennessyMilnerLogic.Formula[A] = {
    //   val (l, succs) = attackTree.rep(node).last
      
    //   l match {
    //     case ConjunctMove() =>
    //       // a Conjunct move should always reach a defender node that then has a set of following attacker nodes 
    //       val conjuncts = for {
    //         defNode <- succs
    //         followingAttack <- attackTree.values(defNode, DefenderMove())
    //       } yield {
    //         attackToHML(followingAttack)
    //       }
    //       HennessyMilnerLogic.And(conjuncts.toList)
    //     case ObservationMove(a) =>
    //       HennessyMilnerLogic.Observe(a, attackToHML(succs.head))
    //     case DefenderMove() =>
    //       throw(new Exception("Reached defender move.."))
    //   }

    // }

    // attackToHML(node)
    accumulatedPrices(node)
  }

  def compute() = {

    val hmlGame = new HMLGame()

    println("hml game size: " + hmlGame.discovered.size)

    val attackerWin = hmlGame.computeWinningRegion()

    val a1 = AttackerObservation(nodes(0), Set(nodes(1)))

    println("lr:" + attackerWin.contains(a1))
    println("rl:" + attackerWin.contains(AttackerObservation(nodes(1), Set(nodes(0)))))
    
    println(buildHML(hmlGame, attackerWin, a1))//.peekPath(a1))

    true
  }
}