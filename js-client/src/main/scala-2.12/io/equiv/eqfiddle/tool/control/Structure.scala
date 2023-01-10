package io.equiv.eqfiddle.tool.control

import scala.collection.mutable.HashMap

import scala.scalajs.js.Date
import scala.scalajs.js

import io.equiv.eqfiddle.tool.arch.Action
import io.equiv.eqfiddle.tool.arch.Control
import io.equiv.eqfiddle.tool.model.NodeID
import io.equiv.eqfiddle.ccs.Interpreter
import io.equiv.eqfiddle.ccs.Syntax
import io.equiv.eqfiddle.ts.WeakTransitionSystem
import io.equiv.eqfiddle.util.Coloring
import io.equiv.eqfiddle.util.Interpreting
import io.equiv.eqfiddle.util.Relation
import io.equiv.eqfiddle.util.LabeledRelation
import io.equiv.eqfiddle.ts.DivergenceInformation
import io.equiv.eqfiddle.algo.AlgorithmLogging
import io.equiv.eqfiddle.spectroscopy.{AbstractSpectroscopy, PositionalSpectroscopy, WeakPositionalSpectroscopy, EdgeSpectroscopy}
import io.equiv.eqfiddle.spectroscopy.FastSpectroscopy
import io.equiv.eqfiddle.hml.ObservationClassFast
import io.equiv.eqfiddle.hml.Spectrum
import io.equiv.eqfiddle.spectroscopy.SpectroscopyInterface
import io.equiv.eqfiddle.algo.WeakTransitionSaturation
import io.equiv.eqfiddle.spectroscopy.EnergyWeakSpectroscopy


class Structure(val main: Control) extends ModelComponent {

  var structure: Structure.TSStructure = null
  var partition: Coloring[NodeID] = Coloring(Map())
  var relation: LabeledRelation[NodeID, String] = LabeledRelation()
  var currentReplayStep: Int = 0
  var currentReplay = List[() => AlgorithmLogging.LogEntry[NodeID]]()

  val operations = HashMap[String, StructureOperation]()

  def init() {
    // registerOperation(new StructureOperation.FixedPointCoupledSimilarityAnalyzer)
    // registerOperation(new StructureOperation.GameCoupledSimilarityAnalyzer)
    // registerOperation(new StructureOperation.GameCoupledSimilarityPlainAnalyzer)

    // registerOperation(new StructureOperation.GameContrasimilarityExponentialAnalyzer)

    // registerOperation(new StructureOperation.SigrefBisimilarityAnalyzer)
    // registerOperation(new StructureOperation.SigrefWeakBisimilarityAnalyzer)

    // registerOperation(new StructureOperation.NaiveTransitiveTauClosure)
    // registerOperation(new StructureOperation.NaiveReflexiveTauClosure)

    // registerOperation(new StructureOperation.WeakStepRelationClosure)

    // registerOperation(new StructureOperation.TauLoopCompressor)
    // registerOperation(new StructureOperation.QuotientBuilder)
  }

  def registerOperation(op: StructureOperation) = {
    operations += (op.slug -> op)
    broadcast(Structure.StructureOperationsChanged(operations))
  }

  override def notify(c: ModelComponent.Change) = c match {
    case Source.SourceChange(source, ast) =>
      val beginInterpret = Date.now
      val interpretationResult =
        new Interpreter(ast, NodeID(_), Structure.arrowAnnotator, Structure.nodeAnnotator, Structure.actionToInput, Structure.actionIsOutput)
        .result(Structure.transitionSystemConstructor(_, _))

      interpretationResult match {
        case p: Interpreting.Problem =>
          println("Interpretation failed after: " + (Date.now - beginInterpret) + "ms.")
          broadcast(Structure.StructureChangeFailed(p))
        case Interpreting.Success(is: Structure.TSStructure) =>
          println("Interpretation took: " + (Date.now - beginInterpret) + "ms.")
          setStructure(is)
      }
    case _ =>
  }

  def setPartition(p: Coloring[NodeID]) = {
    partition = p
    broadcast(Structure.StructurePartitionChange(partition))
  }

  def setRelation(r: Relation[NodeID]): Unit = {
    val labeled = new LabeledRelation(r.tupleSet.map {case (p1, p2) => (p1, "", p2)})
    setRelation(labeled)
  }

  def setRelation(r: LabeledRelation[NodeID, String]): Unit = {
    relation = r
    broadcast(Structure.StructureRelationChange(relation))
  }

  def setReplay(replay: List[() => AlgorithmLogging.LogEntry[NodeID]]): Unit = {
    currentReplay = replay
    currentReplayStep = 0
    broadcast(Structure.StructureReplayChange(currentReplay))
  }

  def doReplayStep(): Boolean = {
    if (currentReplayStep < currentReplay.length) {
      currentReplay(currentReplayStep)() match {
        case AlgorithmLogging.LogRelation(rel, comment) =>
          broadcast(Structure.StructureCommentChange(comment))
          broadcast(Structure.StructureRelationChange(rel))
        case AlgorithmLogging.LogRichRelation(rel, comment) =>
          broadcast(Structure.StructureCommentChange(comment))
          broadcast(Structure.StructureRichRelationChange(rel))
      }
      currentReplayStep += 1
      true
    } else if (currentReplayStep == currentReplay.length) {
      // reset everything
      //broadcast(Structure.StructureRelationChange(relation))
      true
    } else {
      false
    }
  }

  def setStructure(is: Structure.TSStructure) = {
    structure = is
    broadcast(Structure.StructureChange(structure))
    setPartition(Coloring.fromPartition(Set(is.nodes)))
    setRelation(Relation[NodeID]())
  }
}

object Structure {

  type TSStructure = WeakTransitionSystem[NodeID, ActionLabel, NodeLabel]

  abstract sealed class StructureAction extends Action {
    override def implement(target: ModelComponent): Boolean = target match {
      case s: Structure =>
        implementStructure(s)
      case _ =>
        false
    }

    def implementStructure(structure: Structure): Boolean
  }

  case class StructureChange(tsStructure: TSStructure) extends ModelComponent.Change

  case class StructureChangeFailed(problem: Interpreting.Problem) extends ModelComponent.Change

  case class StructurePartitionChange(partition: Coloring[NodeID]) extends ModelComponent.Change

  case class StructureRelationChange(relation: LabeledRelation[NodeID, String]) extends ModelComponent.Change

  case class StructureRichRelationChange(relation: LabeledRelation[(Set[NodeID], String, Set[NodeID]), String]) extends ModelComponent.Change

  case class StructureCommentChange(comment: String) extends ModelComponent.Change

  case class StructureReplayChange(replay: List[() => AlgorithmLogging.LogEntry[NodeID]])
    extends ModelComponent.Change

  case class StructureOperationsChanged(operations: HashMap[String, StructureOperation]) extends ModelComponent.Change {
    override def toString() = operations.map(_._1).mkString
  }

  case class ActionLabel(
      val act: Symbol,
      val x: Option[Double] = None,
      val y: Option[Double] = None) {

    def this(name: String) = {
      this(Symbol(name), None, None)
    }

    val hash = act.hashCode()

    override def hashCode() = hash

    override def equals(other: Any) = other match {
      case o: ActionLabel => this.act == o.act
      case _ => false
    }

    def toActString = {
      act.name
    }

    override def toString() = toActString

    def toStringPairList = {
      act.name ++
      x.toList.map(v => ("x", v.round.toString)) ++
      y.toList.map(v => ("y", v.round.toString))
    }
  }

  case class NodeLabel(
      val act: Set[Symbol],
      val x: Option[Double] = None,
      val y: Option[Double] = None) {

    val hash = act.hashCode()

    override def hashCode() = hash

    override def equals(other: Any) = other match {
      case o: NodeLabel => this.act == o.act
      case _ => false
    }

    def toStringPairList = {
      act.toList.map(v => (v.name, "")) ++
      x.toList.map(v => ("x", v.round.toString)) ++
      y.toList.map(v => ("y", v.round.toString))
    }

  }

  val emptyLabel = NodeLabel(Set())

  val silentLabel = NodeLabel(Set('tau))

  val emptyActionLabel = new ActionLabel("")

  val silentActionLabel = ActionLabel('tau)

  def nodeAnnotator(nodeDecl: Option[Syntax.NodeDeclaration]): Interpreting.Result[NodeLabel] = nodeDecl match {
    case Some(nD @ Syntax.NodeDeclaration(name, attribs, pos)) =>
      val labels = attribs collect { case (k, _) if k != "x" && k != "y" => Symbol(k) }
      try {
        val l = NodeLabel(
            act = labels.toSet,
            x = attribs.find(_._1 == "x").map(_._2.toDouble),
            y = attribs.find(_._1 == "y").map(_._2.toDouble)
        )
        Interpreting.Success(l)
      } catch {
        case e: Exception => Interpreting.Problem(e.toString(), List(nD))
      }
    case None =>
      Interpreting.Success(emptyLabel)
  }

  def arrowAnnotator(arrowLabel: Option[Syntax.Label]): Interpreting.Result[ActionLabel] = arrowLabel match {
    case Some(aL @ Syntax.Label(name, pos)) =>
      try {
        val l = ActionLabel(
          act = Symbol(if (name == "τ") "tau" else name)
        )
        Interpreting.Success(l)
      } catch {
        case e: Exception => Interpreting.Problem(e.toString(), List(aL))
      }
    case None =>
      Interpreting.Success(emptyActionLabel)
  }

  def actionIsOutput(a: ActionLabel): Boolean = actionStrIsOutput(a.toActString)
  def actionToInput(a: ActionLabel): ActionLabel =
    if (actionIsOutput(a)) ActionLabel(Symbol(actionStrToInput(a.toActString)), a.x, a.y) else a

  def actionStrIsOutput(a: String) = a.endsWith("!")
  def actionStrToInput(a: String): String = if (actionStrIsOutput(a)) actionStrToInput(a.dropRight(1)) else a

  def transitionSystemConstructor(
      rel: LabeledRelation[NodeID, ActionLabel],
      labels: Map[NodeID, Structure.NodeLabel],
      oldTs: Option[Structure.TSStructure] = None) = {

    val silentActions = rel.labels filter (_.act == 'tau)

    oldTs match {
      case Some(d: DivergenceInformation[_]) =>
        new WeakTransitionSystem(rel, labels, silentActions.toSet) with DivergenceInformation[NodeID] {
          def diverges(s: NodeID): Boolean = d.asInstanceOf[DivergenceInformation[NodeID]].diverges(s)
        }
      case _ =>
        new WeakTransitionSystem(rel, labels, silentActions.toSet)
    }

  }

  case class StructureCallOperation(slug: String, resetReplay: Boolean = true) extends StructureAction {
    override def implementStructure(structure: Structure) = {
      val op = structure.operations.get(slug)
      for (o <- op) {
        if (resetReplay) {
          structure.setReplay(List())
        }
        o applyOperation structure
      }
      op.isDefined
    }
  }

  case class StructureExamineEquivalences(n1: NodeID, n2: NodeID, resetReplay: Boolean = true) extends StructureAction {

    override def implementStructure(structure: Structure) = {
      if (resetReplay) {
        structure.setReplay(List())
      }
      
      if (structure.structure.nodes(n1) && structure.structure.nodes(n2)) {

        val begin = Date.now

        //val preprocessed = new WeakTransitionSaturation(structure.structure).compute()
        //println("Preprocessed: " + preprocessed)
        val algo = new EnergyWeakSpectroscopy(structure.structure)
        val result = algo.compute(List((n1, n2)), computeFormulas = false)
        println("Spectroscopy took: " + (Date.now - begin) + "ms.")

        for {
          res <- result.relationItems.find(r => r.left == n1 && r.right == n2)
          SpectroscopyInterface.SpectroscopyResultItem(_, _, distinctions, preorderings) = res
        } {
          val dists = distinctions.map(d => d._1.toString() + d._3.map(_.name).mkString(" (", ",", ")")).mkString("<br>")
          val preords = preorderings.map(_.name).mkString("<br>")
          val equations = result.findEqs(n1, n2).map(_.name).mkString("<br>")
          val replay = List(
            () => AlgorithmLogging.LogRelation(result.toPreorderingRelation(), s"Preordered by:<div class='preorderings'>$preords</div>"),
            () => AlgorithmLogging.LogRelation(result.toDistinctionRelation(), s"Distinguished by:<div class='distinctions'>$dists</div>"),
            () => AlgorithmLogging.LogRelation(result.toEquivalencesRelation(), s"Equated by:<div class='equations'>$equations</div>")
          )
          structure.setReplay(replay)
          structure.main.doAction(StructureDoReplayStep(), structure)
        }

        true
      } else {
        val unknownState = if (!structure.structure.nodes(n1)) n1 else n2
        val replay = List(
          () => AlgorithmLogging.LogRelation(LabeledRelation[NodeID, String](), s"Unknown state ‹$unknownState›.")
        )
        structure.setReplay(replay)
        structure.main.doAction(StructureDoReplayStep(), structure)
        false
      }
    }
  }

  case class StructureMinimize(resetReplay: Boolean = true) extends StructureAction {

    override def implementStructure(structure: Structure) = {
      if (resetReplay) {
        structure.setReplay(List())
      }

      val begin = Date.now

      val states = structure.structure.nodes.toList

      val preprocessed = new WeakTransitionSaturation(structure.structure).compute()
      println("Preprocessed: " + preprocessed)
      val algo = new FastSpectroscopy(preprocessed)

      val comparedPairs = for {
        n1i <- 0 until states.length
        n2j <- (n1i + 1) until states.length
      } yield (states(n1i), states(n2j))

      val result = algo.compute(comparedPairs, computeFormulas = false)
      println("Minimization Spectroscopy took: " + (Date.now - begin) + "ms.")

      val distRel = result.toDistancesRelation()
      val lubDists = distRel.tupleSet//.map { case (l, dists, r) => (l, dists.reduce(_ lub _), r) }

      val eqLevels =
        distRel.labels.map(result.spectrum.getStrongestPreorderClassFromClass(_))
        .flatten.toSet[Spectrum.EquivalenceNotion[ObservationClassFast]].toList.sortBy(_.obsClass.toTuple).reverse

      val replay = for {
        Spectrum.EquivalenceNotion(name, obsClass) <- eqLevels//equivMessages
        resultRelation = for {
          (p, d, q) <- lubDists
          if !d.exists(_ <= obsClass)
        } yield (p, "eq", q)
        msg = obsClass.toTuple.toString().replace(Int.MaxValue.toString(), "∞") + " " + name
      } yield () => AlgorithmLogging.LogRelation(new LabeledRelation(resultRelation), msg)

      structure.setReplay(replay.toList)
      structure.main.doAction(StructureDoReplayStep(), structure)

      true
    }
  }

  case class StructureCharacterize(node: NodeID, resetReplay: Boolean = true) extends StructureAction {

    override def implementStructure(structure: Structure) = {
      if (resetReplay) {
        structure.setReplay(List())
      }

      if (structure.structure.nodes(node)) {

        val begin = Date.now

        val algo = new FastSpectroscopy(structure.structure)

        val comparedPairs = for {
          n2 <- structure.structure.nodes
        } yield (node, n2)

        val result = algo.compute(comparedPairs, computeFormulas = false)
        println("Characterization Spectroscopy took: " + (Date.now - begin) + "ms.")

        for {
          res <- result.relationItems//.find(r => r.left == n1 && r.right == n2)
          SpectroscopyInterface.SpectroscopyResultItem(_, _, distinctions, preorderings) = res
        } {
          val dists = distinctions.map(d => d._1.toString() + d._3.map(_.name).mkString(" (", ",", ")")).mkString("<br>")
          val preords = preorderings.map(_.name).mkString("<br>")
          //val equations = result.findEqs(n1, n2).map(_.name).mkString("<br>")
          val replay = List(
            () => AlgorithmLogging.LogRelation(result.toPreorderingRelation(), s"Preordered by:<div class='preorderings'>$preords</div>"),
            () => AlgorithmLogging.LogRelation(result.toDistinctionRelation(), s"Distinguished by:<div class='distinctions'>$dists</div>"),
            //() => AlgorithmLogging.LogRelation(result.toEquivalencesRelation(), s"Equated by:<div class='equations'>$equations</div>")
          )
          structure.setReplay(replay)
          structure.main.doAction(StructureDoReplayStep(), structure)
        }

        true
      } else {
        structure.setReplay(List(
          () => AlgorithmLogging.LogRelation(LabeledRelation[NodeID, String](), s"Unknown state ‹$node›.")
        ))
        structure.main.doAction(StructureDoReplayStep(), structure)
        false
      }
    }
  }

  case class StructureDoReplayStep(goToStep: Int = -1) extends StructureAction {
    override def implementStructure(structure: Structure) = {
      if (goToStep >= 0) structure.currentReplayStep = goToStep
      structure.doReplayStep()
    }
  }

}