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

  type Notion <: ObservationNotion
  type GamePosition <: SimpleGame.GamePosition
  type MaterializedPosition = MaterializedGamePosition[GamePosition, Energy]
  type SpectroscopyGame <: SimpleGame[GamePosition] with AbstractGameDiscovery[GamePosition] with EnergyGame[GamePosition]

  val ts: WeakTransitionSystem[S, A, L]
  val spectrum: Spectrum[Notion]
  var gameSize: (Int, Int) = (0, 0)

  def notionToEnergy(obsNotion: Notion): Energy
  def energyToNotion(e: Energy): Notion

  /** Place to memoize distinguishing formulas per position */
  val distinguishingFormulas =
    collection.mutable.Map[(GamePosition, Energy), Iterable[CF]]()

  /** Construct a spectroscopy game object */
  def buildSpectroscopyGame(configuration: SpectroscopyInterface.SpectroscopyConfig = SpectroscopyInterface.SpectroscopyConfig()): SpectroscopyGame
  
  def relationItemToGamePosition(p: S, q: S): GamePosition
  def gamePositionToRelationItem(gp: GamePosition): Option[(S, S)]

  def buildHMLWitness(
    game: SpectroscopyGame,
    node: GamePosition,
    price: Energy
  ): Iterable[CF]

  def compute(
    comparedPairs: Iterable[(S,S)],
    config: SpectroscopyInterface.SpectroscopyConfig = SpectroscopyInterface.SpectroscopyConfig()
  ) : SpectroscopyInterface.SpectroscopyResult[S, A, Notion, CF] = {

    debugLog(s"Start spectroscopy on ${ts.nodes.size} node transition system with ${comparedPairs.size} compared pairs.")

    val spectroscopyGame = buildSpectroscopyGame(config)

    val init = for {
      (p, q) <- comparedPairs
      start <- List(relationItemToGamePosition(p, q), relationItemToGamePosition(q, p))
    } yield start

    debugLog("HML spectroscopy game construction ...")

    spectroscopyGame.populateGame(init)

    debugLog("HML spectroscopy game size: " + spectroscopyGame.discovered.size)

    if (config.computeFormulas) {
      for {
        gn <- init
        (p, q) <- gamePositionToRelationItem(gn)
        bestPrice <- spectroscopyGame.attackerWinningBudgets(gn)
        witnessFormula <- buildHMLWitness(spectroscopyGame, gn, bestPrice)
      } {
        debugLog("Distinguished under " + spectrum.classifyFormula(witnessFormula) + " preorder by " + witnessFormula.toString())
        checkDistinguishing(witnessFormula, p, q)
      }
      val distinguishingNodeFormulas = for {
        (node, pricedFormulas) <- distinguishingFormulas
          .toSet[((GamePosition, Energy), Iterable[CF])]
          .groupBy(kv => kv._1._1)
        formulas = for {
          (_, formulasForPrice) <- pricedFormulas
          f <- spectrum.getLeastDistinguishing(formulasForPrice)
        } yield f
      } yield (node, formulas)

      val bisimilarNodes = for {
        gn <- spectroscopyGame.discovered
        if (gamePositionToRelationItem(gn).isDefined) &&
          (!spectroscopyGame.attackerWinningBudgets.isDefinedAt(gn) || spectroscopyGame.attackerWinningBudgets(gn).isEmpty)
      } yield (gn, Set[CF]())

      val distinguishingNodeFormulasExtended = distinguishingNodeFormulas ++ bisimilarNodes

      val gameString = debugLog(
        graphvizGameWithFormulas(spectroscopyGame, spectroscopyGame.attackerWinningBudgets.toMap, distinguishingNodeFormulasExtended),
        asLink = "https://edotor.net/?engine=dot#"
      )

      val bestPreorders: Map[GamePosition,List[Spectrum.EquivalenceNotion[Notion]]] =
        distinguishingNodeFormulasExtended.mapValues { ffs =>
        val classes = ffs.flatMap(spectrum.classifyFormula(_)._2)
        spectrum.getStrongestPreorderClass(classes)
      }

      val spectroResults = for {
        gn <- spectroscopyGame.discovered
        (p, q) <- gamePositionToRelationItem(gn)
        preorders <- bestPreorders.get(gn)
        distinctionFormulas = distinguishingNodeFormulasExtended(gn)
        distinctions = for {
          f <- distinctionFormulas.toList
          (price, eqs) = spectrum.classifyFormula(f)
        } yield (f, price, eqs)
      } yield SpectroscopyInterface.SpectroscopyResultItem[S, A, Notion, CF](p, q, distinctions, preorders)

      if (config.saveGameSize) gameSize = spectroscopyGame.gameSize()

      SpectroscopyInterface.SpectroscopyResult[S, A, Notion, CF](spectroResults.toList, spectrum, meta = Map("game" -> gameString))
    } else {
      for {
        gn <- init
        (p, q) <- gamePositionToRelationItem(gn)
      } {
        spectroscopyGame.attackerWinningBudgets(gn)
      }

      // handle bisimilar nodes
      val bisimilarNodes = for {
        gn <- spectroscopyGame.discovered
        if (gamePositionToRelationItem(gn).isDefined) &&
          (!spectroscopyGame.attackerWinningBudgets.isDefinedAt(gn) || spectroscopyGame.attackerWinningBudgets(gn).isEmpty)
      } {
        spectroscopyGame.attackerWinningBudgets(gn) = List()
      }

      val bestPreorders: Map[GamePosition,(Set[Notion],List[Spectrum.EquivalenceNotion[Notion]])] =
        spectroscopyGame.attackerWinningBudgets.toMap.mapValues { energies =>
        val fcs = energies.toSet[Energy].map(energyToNotion(_))
        (fcs, spectrum.getStrongestPreorderClassFromClass(fcs))
      }

      val spectroResults = for {
        gn <- spectroscopyGame.discovered
        (p, q) <- gamePositionToRelationItem(gn)
        (prices, preorders) <- bestPreorders.get(gn)
        distinctions = for {
          price <- prices
        } yield (HennessyMilnerLogic.True[A].asInstanceOf[CF], price, spectrum.classifyClass(price))
      } yield SpectroscopyInterface.SpectroscopyResultItem[S, A, Notion, CF](p, q, distinctions.toList, preorders)

      if (config.saveGameSize) gameSize = spectroscopyGame.gameSize()

      SpectroscopyInterface.SpectroscopyResult[S, A, Notion, CF](spectroResults.toList, spectrum)
    }

  }


  def checkIndividualPreorder(
    comparedPairs: Iterable[(S,S)],
    notion: String,
    config: SpectroscopyInterface.SpectroscopyConfig = SpectroscopyInterface.SpectroscopyConfig()
  ) : SpectroscopyInterface.IndividualNotionResult[S]

  def materializedToBaseGamePosition(gn: MaterializedPosition) = gn match {
    case MaterializedAttackerPosition(bgn, e) =>
      bgn
    case MaterializedDefenderPosition(bgn, e) =>
      bgn
  }

  def graphvizMaterializedGame(
    game: MaterializedEnergyGame[GamePosition, Energy],
    attackerWin: Set[MaterializedPosition]
  ): String

  def checkDistinguishing(formula: HennessyMilnerLogic.Formula[A], p: S, q: S) = {
    val hmlInterpreter = new HMLInterpreter(ts)
    val check = hmlInterpreter.isTrueAt(formula, List(p, q))
    if (!check(p) || check(q)) {
      AlgorithmLogging.debugLog("Formula " + formula.toString() + " is no sound distinguishing formula! " + check, logLevel = 4)
    }
  }

  def gamePositionToString(gn: GamePosition): String
  def gamePositionToID(gn: GamePosition): String

  def graphvizGameWithFormulas(
    spectroscopyGame: SpectroscopyGame,
    attackerWinningBudgets: Map[GamePosition, Iterable[Energy]],
    formulas: Map[GamePosition, Set[CF]]
  ) = {
    val visualizer = new GameGraphVisualizer(spectroscopyGame) {

      def positionToID(gn: GamePosition): String =
        gamePositionToID(gn)

      def positionToString(gn: GamePosition): String = {
        val budgetString = attackerWinningBudgets.getOrElse(gn,Set()).map(_.vector.mkString("(",",",")")).mkString(" / ")
        val formulaString = formulas.getOrElse(gn,Set()).mkString("\\n").replaceAllLiterally("⟩⊤","⟩")
        gamePositionToString(gn) +
         (if (budgetString != "") s"\\n------\\n$budgetString" else "") +
         (if (formulaString != "") s"\\n------\\n$formulaString" else "")
      }

      def moveToLabel(gn1: GamePosition, gn2: GamePosition) = spectroscopyGame.weight(gn1, gn2).toString()
    }

    val attackerWin = attackerWinningBudgets.filter(_._2.nonEmpty).keySet.toSet

    visualizer.outputDot(attackerWin)
  }

}

object SpectroscopyInterface {

  case class SpectroscopyConfig(
    val useCleverSpectroscopyGame: Boolean = true,
    val useBranchingSpectroscopyGame: Boolean = true,
    val useSymmetryPruning: Boolean = true,
    val useBisimMinimization: Boolean = false,
    val useCleverInstanceBranching: Boolean = true,
    val computeFormulas: Boolean = false,
    val saveGameSize: Boolean = true,
    val energyCap: Int = Int.MaxValue
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