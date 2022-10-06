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
import de.bbisping.eqfiddle.hml.Spectrum
import de.bbisping.eqfiddle.hml.HMLInterpreter
import de.bbisping.eqfiddle.util.Coloring

abstract class AbstractSpectroscopy[S, A, L, CF <: HennessyMilnerLogic.Formula[A]] (
    val ts: WeakTransitionSystem[S, A, L],
    val nodes: List[S])
  extends AlgorithmLogging[S] {

  import AbstractSpectroscopy._

  def spectrum: Spectrum[ObservationClass]

  def buildStrategyFormulas(game: AbstractSpectroscopyGame[S, A, L])(node: GameNode, possibleMoves: Iterable[Set[CF]]): Set[CF]

  def pruneDominated(oldFormulas: Set[CF]): Set[CF]

  def compute(): SpectroscopyResult[S, A, ObservationClass, CF]

  /* Discards distinguishing formulas that do not contribute “extreme” distinguishing notions of equivalence */
  val discardLanguageDominatedResults: Boolean = false

  def nodeIsRelevantForResults(game: AbstractSpectroscopyGame[S, A, L], gn: GameNode): Boolean

  def collectSpectroscopyResult(
    game: AbstractSpectroscopyGame[S, A, L],
    nodeFormulas: Map[GameNode, Set[CF]])
  : SpectroscopyResult[S, A, ObservationClass, CF] = {
    
    val bestPreorders: Map[GameNode,List[Spectrum.EquivalenceNotion[ObservationClass]]] = nodeFormulas.mapValues { ffs =>
      val classes = ffs.flatMap(spectrum.classifyFormula(_)._2)
      spectrum.getStrongestPreorderClass(classes)
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
        (price, eqs) = spectrum.classifyFormula[CF](f)
      } yield (f, price, eqs)
    } yield SpectroscopyResultItem[S, A, ObservationClass, CF](p, q, distinctions, preorders)

    SpectroscopyResult(spectroResults.toList, spectrum)
  }

  def checkDistinguishing(formula: CF, p: S, q: S) = {
    val hmlInterpreter = new HMLInterpreter(ts)
    val check = hmlInterpreter.isTrueAt(formula, List(p, q))
    if (!check(p) || check(q)) {
      System.err.println("Formula " + formula.toString() + " is no sound distinguishing formula! " + check)
    }
  }

  def gameEdgeToLabel(game: AbstractSpectroscopyGame[S, A, L], gn1: GameNode, gn2: GameNode): String

  def graphvizGameWithFormulas(game: AbstractSpectroscopyGame[S, A, L], win: Set[GameNode], formulas: Map[GameNode, Set[CF]]) = {
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

  case class SpectroscopyResultItem[S, A, +OC <: ObservationClass, +OF <: HennessyMilnerLogic.Formula[A]](
    left: S,
    right: S,
    distinctions: List[(OF, OC, List[Spectrum.EquivalenceNotion[OC]])],
    preorderings: List[Spectrum.EquivalenceNotion[OC]]
  ) {
    def serialize(listConstructor: (Iterable[Any] => Any), mapConstructor: (Map[String, Any] => Any)) = {
      val dists = for {
        (f, price, eqs) <- distinctions
      } yield mapConstructor(Map(
        ("formula", f.toString()),
        ("price", listConstructor(price.toTuple.productIterator.toIterable)),
        ("inequivalences", listConstructor(eqs.map(_.name))))
      )
      mapConstructor(Map(
        ("left", left.toString()),
        ("right", right.toString()),
        ("distinctions", listConstructor(dists)),
        ("preorderings", listConstructor(preorderings.map(_.name)))
      ))
    }
  }

  case class SpectroscopyResult[S, A, +OC <: ObservationClass, +OF <:HennessyMilnerLogic.Formula[A]](
      val relationItems: List[SpectroscopyResultItem[S, A, OC, OF]], val spectrum: Spectrum[OC]) {

    def resultFor(p: S, q: S) = {
      for {
        res <- relationItems
        if res.left == p && res.right == q
      } yield res
    }

    def toDistinctionRelation() = {
      val relTuples = for {
        SpectroscopyResultItem(l, r, dists, preords) <- relationItems
        dis <- dists
        inEqs = dis._3.map(_.name).mkString(" (", ",", ")")
      } yield (l, dis._1.toString() + inEqs, r)
      new LabeledRelation(relTuples.toSet)
    }

    def toPreorderingRelation() = {
      val relTuples = for {
        SpectroscopyResultItem(l, r, dists, preords) <- relationItems
        pre <- preords
      } yield (l, pre.name.toString(), r)
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
      } yield (p, eq.name + " eq", q)
      new LabeledRelation(relTuples.toSet)
    }

    def toDistancesRelation[OCC >: OC](): LabeledRelation[S,Set[OCC]] = {
      val undirectedResults = relationItems.groupBy(r => Set(r.left, r.right))
      val undirectedDistinctions = for {
        (pq, results) <- undirectedResults
        if pq.size == 2
      } yield (pq, results.flatMap(_.distinctions.map(_._2)).toSet)
      val relTuples = for {
        (pq, dist) <- undirectedDistinctions
        orderedPair = pq.toList
        p = orderedPair(0)
        q = orderedPair(1)
      } yield (p, dist.map(_.asInstanceOf[OCC]), q)
      new LabeledRelation(relTuples.toSet)
    }

    def toQuotients(eqs: Iterable[Spectrum.EquivalenceNotion[ObservationClass]], rep: (S, S) => S): Iterable[Coloring[S]] = {
      val distances = toDistancesRelation().symmetricClosure
      for {
        eq <- eqs
      } yield {
        distances.toQuotientColoring(
          dist => !dist.exists(d => d <= eq.obsClass),
          rep
        )
      }
    }

    def foundPreorders(p: S, q: S): List[Spectrum.EquivalenceNotion[OC]] = {
      for {
        res <- resultFor(p, q)
        preord <- res.preorderings
      } yield preord
    }

    def foundDistinctions(p: S, q: S): List[Spectrum.EquivalenceNotion[OC]] = {
      for {
        res <- resultFor(p, q)
        dists <- res.distinctions
        dis <- dists._3
      } yield dis
    }

    def findEqs(p: S, q: S): List[Spectrum.EquivalenceNotion[OC]] = {
      val distinctionClasses = for {
        res <- relationItems
        if (res.left == p && res.right == q) || (res.left == q && res.right == p)
        dists <- res.distinctions
        disClass <- dists._3
      } yield disClass
      spectrum.getStrongestPreorderClass(distinctionClasses)
    }

    def printStats(): String = {
      s"Relation Items: ${relationItems.size}"
    }

  }

}