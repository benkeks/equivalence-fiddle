package de.bbisping.eqfiddle.spectroscopy

import de.bbisping.eqfiddle.util.Relation
import de.bbisping.eqfiddle.ts.WeakTransitionSystem
import de.bbisping.eqfiddle.util.FixedPoint
import de.bbisping.eqfiddle.game.WinningRegionComputation
import de.bbisping.eqfiddle.game.AttackGraphBuilder
import de.bbisping.eqfiddle.game.SimpleGame.GameNode
import de.bbisping.eqfiddle.algo.AlgorithmLogging
import de.bbisping.eqfiddle.util.LabeledRelation
import de.bbisping.eqfiddle.game.GameGraphVisualizer
import de.bbisping.eqfiddle.hml.HennessyMilnerLogic
import de.bbisping.eqfiddle.hml.ObservationClass
import de.bbisping.eqfiddle.hml.HMLInterpreter

abstract class AbstractSpectroscopy[S, A, L] (
    val ts: WeakTransitionSystem[S, A, L],
    val nodes: List[S])
  extends AlgorithmLogging[S] {

  import AbstractSpectroscopy._

  def buildStrategyFormulas(game: AbstractSpectroscopyGame[S, A, L])(node: GameNode, possibleMoves: Iterable[Set[HennessyMilnerLogic.Formula[A]]]): Set[HennessyMilnerLogic.Formula[A]]

  def pruneDominated(oldFormulas: Set[HennessyMilnerLogic.Formula[A]]): Set[HennessyMilnerLogic.Formula[A]]

  def compute(): SpectroscopyResult[S,A]

  /* Discards distinguishing formulas that do not contribute “extreme” distinguishing notions of equivalence */
  val discardLanguageDominatedResults: Boolean = true

  def nodeIsRelevantForResults(game: AbstractSpectroscopyGame[S, A, L], gn: GameNode): Boolean

  def collectSpectroscopyResult(
    game: AbstractSpectroscopyGame[S, A, L],
    nodeFormulas: Map[GameNode, Set[HennessyMilnerLogic.Formula[A]]])
  : SpectroscopyResult[S,A] = {
    
    val bestPreorders: Map[GameNode,List[ObservationClass.EquivalenceNotion]] = nodeFormulas.mapValues { ffs =>
      val classes = ffs.flatMap(_.classifyFormula()._2)
      ObservationClass.getStrongestPreorderClass(classes)
    }

    val spectroResults = for {
      gn <- game.discovered
      if gn.isInstanceOf[game.AttackerObservation]
      game.AttackerObservation(p, qq, kind) = gn
      if nodeIsRelevantForResults(game, gn)
      q <- qq
      preorders <- bestPreorders.get(gn)
      distinctionFormulas = nodeFormulas(gn)
      distinctions = for {
        f <- distinctionFormulas.toList
        (price, eqs) = f.classifyFormula()
      } yield (f, price, eqs)
    } yield SpectroscopyResultItem(p, q, distinctions, preorders)

    SpectroscopyResult(spectroResults.toList)
  }

  def checkDistinguishing(formula: HennessyMilnerLogic.Formula[A], p: S, q: S) = {
    val hmlInterpreter = new HMLInterpreter(ts)
    val check = hmlInterpreter.isTrueAt(formula, List(p, q))
    if (!check(p) || check(q)) {
      System.err.println("Formula " + formula.toString() + " is no sound distinguishing formula! " + check)
    }
  }

  def gameEdgeToLabel(game: AbstractSpectroscopyGame[S, A, L], gn1: GameNode, gn2: GameNode): String

  def graphvizGameWithFormulas(game: AbstractSpectroscopyGame[S, A, L], win: Set[GameNode], formulas: Map[GameNode, Set[HennessyMilnerLogic.Formula[A]]]) = {
    val visualizer = new GameGraphVisualizer(game) {

      def nodeToID(gn: GameNode): String = gn.hashCode().toString()

      def nodeToString(gn: GameNode): String = {
        val formulaString = formulas.getOrElse(gn,Set()).mkString("\\n").replaceAllLiterally("⟩⊤","⟩")
        (gn match {
          case game.AttackerObservation(p, qq: Set[_], kind) =>
            val qqString = qq.mkString("{",",","}")
            s"$p, $qqString, $kind"
          case game.DefenderConjunction(p, qqPart: List[Set[_]]) =>
            val qqString = qqPart.map(_.mkString("{",",","}")).mkString("/")
            s"$p, $qqString"
          case _ => ""
        }).replaceAllLiterally(".0", "") + (if (formulaString != "") s"\\n------\\n$formulaString" else "")
      }

      def edgeToLabel(gn1: GameNode, gn2: GameNode) = gameEdgeToLabel(game, gn1, gn2)
    }

    visualizer.outputDot(win)
  }

}

object AbstractSpectroscopy {

  case class SpectroscopyResultItem[S, A](
    left: S,
    right: S,
    distinctions: List[(HennessyMilnerLogic.Formula[A], ObservationClass, List[ObservationClass.EquivalenceNotion])],
    preorderings: List[ObservationClass.EquivalenceNotion]
  ) {
    def serialize(listConstructor: (Iterable[Any] => Any), mapConstructor: (Map[String, Any] => Any)) = {
      val dists = for {
        (f, price, eqs) <- distinctions
      } yield mapConstructor(Map(
        ("formula", f.toString()),
        ("price", listConstructor(price.productIterator.toIterable)),
        ("inequivalences", listConstructor(eqs.map(_._1))))
      )
      mapConstructor(Map(
        ("left", left.toString()),
        ("right", right.toString()),
        ("distinctions", listConstructor(dists)),
        ("preorderings", listConstructor(preorderings.map(_._1)))
      ))
    }
  }

  case class SpectroscopyResult[S, A](val relationItems: List[SpectroscopyResultItem[S, A]]) {

    def toDistinctionRelation() = {
      val relTuples = for {
        SpectroscopyResultItem(l, r, dists, preords) <- relationItems
        dis <- dists
        inEqs = dis._3.map(_._1).mkString(" (", ",", ")")
      } yield (l, dis._1.toString() + inEqs, r)
      new LabeledRelation(relTuples.toSet)
    }

    def toPreorderingRelation() = {
      val relTuples = for {
        SpectroscopyResultItem(l, r, dists, preords) <- relationItems
        pre <- preords
      } yield (l, pre._1.toString(), r)
      new LabeledRelation(relTuples.toSet)
    }

    def toEquivalencesRelation() = {
      val undirectedTuples = {
        for {
          SpectroscopyResultItem(l, r, _, _) <- relationItems
          if l != r
        } yield Set(l, r)
      }.toSet
      val relTuples = for {
        pair <- undirectedTuples
        orderedPair = pair.toList
        p = orderedPair(0)
        q = orderedPair(1)
        eq <- findEqs(p, q)
      } yield (p, eq._1 + " eq", q)
      new LabeledRelation(relTuples.toSet)
    }

    def foundPreorders(p: S, q: S): List[ObservationClass.EquivalenceNotion] = {
      for {
        res <- relationItems
        if res.left == p && res.right == q
        preord <- res.preorderings
      } yield preord
    }

    def foundDistinctions(p: S, q: S): List[ObservationClass.EquivalenceNotion] = {
      for {
        res <- relationItems
        if res.left == p && res.right == q
        dists <- res.distinctions
        dis <- dists._3
      } yield dis
    }

    def findEqs(p: S, q: S): List[(String, ObservationClass)] = {
      val distinctionClasses = for {
        res <- relationItems
        if (res.left == p && res.right == q) || (res.left == q && res.right == p)
        dists <- res.distinctions
        disClass <- dists._3
      } yield disClass
      ObservationClass.getStrongestPreorderClass(distinctionClasses)
    }

  }

}