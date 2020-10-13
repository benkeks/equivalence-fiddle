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
import de.bbisping.coupledsim.hml.HMLInterpreter
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
        if (qq0.size == 1) {
          // wlog only have negation moves when the defender is focused (which can be forced by the attacker using a preceding conjunction)
          val neg = AttackerObservation(qq0.head, Set(p0))
          recordedMoveEdges((gn, neg)) = NegationMove()
          dn ++ List(neg)
        } else {
          // conjunct moves only make sense if the defender is spread
          val conj = DefenderConjunction(p0, qq0)
          recordedMoveEdges((gn, conj)) = ConjunctMove()
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
    }
  }

  def buildHML(game: HMLGame, win: Set[GameNode], node: GameNode) = {

    val attackTreeBuilder = new AttackTreeBuilder[Set[HennessyMilnerLogic.Formula[A]]]()

    // Note: this actually does not need to be a tree at this point
    val attackTree = attackTreeBuilder.buildAttackTree(game, win, node)

    println("Attack Tree: " + attackTree.tupleSet.mkString("\n"))

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
          ff.map(HennessyMilnerLogic.Negate(_))
        case ObservationMove(a) =>
          ff.map(HennessyMilnerLogic.Observe(a, _))
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
        val cl = f.getRootClass()
        if !formulaClasses.exists(clOther => cl.above(clOther) && cl != clOther)
      } yield {
        f
      }
    }

    val accumulatedPrices = attackTreeBuilder.accumulatePrices(
      tree = attackTree,
      priceCons = moveToHML _,
      pricePick = mergeMoves _,
      lowerPrice = lubMoves,
      supPrice = Set(),
      node = node,
      targetRegion = defenderDefeats
    )

    val formulas = accumulatedPrices(node)

    if (AlgorithmLogging.loggingActive) {

      def gameNodeToTuple(n: SimpleGame.GameNode) = n match {
        case AttackerObservation(p, qq) => 
          (Set(p), "A", qq)
        case DefenderConjunction(p, qq) => 
          (Set(p), "D", qq)
      }
      
      val rel: Set[((Set[S], String, Set[S]), String, (Set[S], String, Set[S]))] = for {
        (n1, n2) <- attackTree.tupleSet
      } yield (gameNodeToTuple(n1), recordedMoveEdges(n1, n2).toString(), gameNodeToTuple(n2))
      
      val msg = for {
        f <- formulas
        s = gameNodeToTuple(node)
        p <- s._1.headOption
        q <- s._3.headOption
      } yield {
        p + " distinguished from " + q + " under " + f.classifyFormula() + " preorder by " + f.toString()
      }

      logRichRelation(new LabeledRelation(rel), msg.mkString("<br>\n"))
    }

    formulas
  }

  def compute() = {

    val hmlGame = new HMLGame()

    println("hml game size: " + hmlGame.discovered.size)

    val attackerWin = hmlGame.computeWinningRegion()
    val aLR = AttackerObservation(nodes(0), Set(nodes(1)))
    val aRL = AttackerObservation(nodes(1), Set(nodes(0)))

    val hmlInterpreter = new HMLInterpreter(ts)

    println("left simBy right:" + !attackerWin.contains(aLR))
    if (attackerWin.contains(aLR)) {
      val formulas = buildHML(hmlGame, attackerWin, aLR)
      formulas.foreach { f =>
        println("Distinguished under " + f.classifyFormula() + " preorder by " + f.toString())
        val check = hmlInterpreter.isTrueAt(f, List(nodes(0), nodes(1)))
        if (!check(nodes(0)) || check(nodes(1))) {
          System.err.println("Formula " + f.toString() + " is no sound distinguishing formula! " + check)
        }
      }
    }

    println("right simBy left:" + !attackerWin.contains(aRL))
    if (attackerWin.contains(aRL)) {
      val formulas = buildHML(hmlGame, attackerWin, aRL)
      formulas.foreach { f =>
        println("Distinguished under " + f.classifyFormula() + " preorder by " + f.toString())
        val check = hmlInterpreter.isTrueAt(f, List(nodes(0), nodes(1)))
        if (!check(nodes(1)) || check(nodes(0))) {
          System.err.println("Formula " + f.toString() + " is no sound distinguishing formula! " + check)
        }
      }
    }

    true
  }
}