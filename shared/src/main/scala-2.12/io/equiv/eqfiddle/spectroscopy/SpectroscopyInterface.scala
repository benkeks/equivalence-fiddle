package io.equiv.eqfiddle.spectroscopy

import io.equiv.eqfiddle.util.Relation
import io.equiv.eqfiddle.util.LabeledRelation
import io.equiv.eqfiddle.util.Coloring

import io.equiv.eqfiddle.algo.AlgorithmLogging

import io.equiv.eqfiddle.hml.HennessyMilnerLogic
import io.equiv.eqfiddle.hml.HMLInterpreter
import io.equiv.eqfiddle.hml.ObservationNotion
import io.equiv.eqfiddle.hml.Spectrum

import io.equiv.eqfiddle.game.SimpleGame
import io.equiv.eqfiddle.game.AbstractGameDiscovery
import io.equiv.eqfiddle.game.GameGraphVisualizer
import io.equiv.eqfiddle.game.EnergyGame.Energy
import io.equiv.eqfiddle.game.MaterializedEnergyGame
import io.equiv.eqfiddle.game.MaterializedEnergyGame._

import io.equiv.eqfiddle.ts.WeakTransitionSystem
import io.equiv.eqfiddle.game.EnergyGame

trait SpectroscopyInterface[S, A, L, CF <: HennessyMilnerLogic.Formula[A]]
    extends AlgorithmLogging[S]{

  /** The transition system to be analyzed. */
  val ts: WeakTransitionSystem[S, A, L]

  /** The type of equivalence notions the spectroscopy refers to */
  type Notion <: ObservationNotion

  /** The spectrum of notions we are working with. */
  val spectrum: Spectrum[Notion]

  /** Decide all behavioral preorders of the spectrum for the compared pairs. */
  def decideAll(
    comparedPairs: Iterable[(S,S)],
    config: SpectroscopyInterface.SpectroscopyConfig = SpectroscopyInterface.SpectroscopyConfig()
  ) : SpectroscopyInterface.SpectroscopyResult[S, A, Notion, CF]

  /** Decide an individual notion preorder for the compared pairs. */
  def checkIndividualPreorder(
      comparedPairs: Iterable[(S,S)],
      notion: String,
      config: SpectroscopyInterface.SpectroscopyConfig = SpectroscopyInterface.SpectroscopyConfig()
  ) : SpectroscopyInterface.IndividualNotionResult[S]
}

object SpectroscopyInterface {

  case class SpectroscopyConfig(
    val useCleverSpectroscopyGame: Boolean = true,
    val useBranchingSpectroscopyGame: Boolean = true,
    val useSymmetryPruning: Boolean = true,
    val useCleverInstanceBranching: Boolean = true,
    val computeFormulas: Boolean = false,
    val saveGameSize: Boolean = true,
    val energyCap: Int = Int.MaxValue,
    val useBisimMinimization: Boolean = false
  )

  case class SpectroscopyResultItem[S, A, +OC <: ObservationNotion, +OF <: HennessyMilnerLogic.Formula[A]](
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

  case class SpectroscopyResult[S, A, +OC <: ObservationNotion, +OF <:HennessyMilnerLogic.Formula[A]](
      val relationItems: List[SpectroscopyResultItem[S, A, OC, OF]],
      val spectrum: Spectrum[OC],
      val meta: Map[String, String] = Map()) {

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

    def toQuotients(eqs: Iterable[Spectrum.EquivalenceNotion[ObservationNotion]], rep: (S, S) => S, initialApproximation: Iterable[(S,S)]): Iterable[Coloring[S]] = {
      val distances = toDistancesRelation().symmetricReflexiveClosure(initialApproximation.map(_._1), Set())
      for {
        eq <- eqs
      } yield {
        distances.toQuotientColoring(
          dist => !dist.exists(d => d <= eq.obsNotion),
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

    def foundImpliedPreorders(eqs: Iterable[Spectrum.EquivalenceNotion[ObservationNotion]], p: S, q: S): Iterable[Spectrum.EquivalenceNotion[ObservationNotion]] = {
      val preords = foundPreorders(p, q)
      for {
        eq <- eqs
        if preords.exists(p => eq.obsNotion <= p.obsNotion)
      } yield eq
    }

    def foundDistinctions(p: S, q: S): List[Spectrum.EquivalenceNotion[OC]] = {
      for {
        res <- resultFor(p, q)
        dists <- res.distinctions
        dis <- dists._3
      } yield dis
    }

    def foundDistinctionsWithCertificate(p: S, q: S): List[(OF, List[Spectrum.EquivalenceNotion[OC]])] = {
      for {
        res <- resultFor(p, q)
        dists <- res.distinctions
      } yield (dists._1, dists._3)
    }

    def foundDistinctionCoordinates(p: S, q: S): List[OC] = {
      for {
        res <- resultFor(p, q)
        dists <- res.distinctions
      } yield (dists._2)
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

  case class IndividualNotionResult[S](
    items: Iterable[IndividualNotionResultItem[S]],
    relation: Set[(S, String, S)],
    val meta: Map[String, String] = Map())

  case class IndividualNotionResultItem[S](left: S, right: S, isMaintained: Boolean)

}