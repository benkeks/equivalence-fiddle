package de.bbisping.coupledsim.tool.control

import scala.collection.mutable.HashMap
import scala.scalajs.js.Date
import de.bbisping.coupledsim.tool.arch.Action
import de.bbisping.coupledsim.tool.arch.Control
import de.bbisping.coupledsim.tool.model.NodeID
import de.bbisping.coupledsim.ts.Interpreter
import de.bbisping.coupledsim.ts.Syntax
import de.bbisping.coupledsim.ts.WeakTransitionSystem
import de.bbisping.coupledsim.util.Coloring
import de.bbisping.coupledsim.util.Interpreting
import de.bbisping.coupledsim.util.Relation
import de.bbisping.coupledsim.util.LabeledRelation
import de.bbisping.coupledsim.ts.DivergenceInformation
import de.bbisping.coupledsim.algo.AlgorithmLogging

class Structure(val main: Control) extends ModelComponent {

  var structure: Structure.TSStructure = null
  var partition: Coloring[NodeID] = Coloring(Map())
  var relation: LabeledRelation[NodeID, String] = LabeledRelation()
  var currentReplayStep: Int = 0
  var currentReplay = List[() => AlgorithmLogging.LogEntry[NodeID, Structure.ActionLabel, Structure.NodeLabel]]()

  val operations = HashMap[String, StructureOperation]()

  def init() {
    registerOperation(new StructureOperation.FixedPointCoupledSimilarityAnalyzer)
    registerOperation(new StructureOperation.GameCoupledSimilarityAnalyzer)
    registerOperation(new StructureOperation.GameCoupledSimilarityPlainAnalyzer)

    registerOperation(new StructureOperation.GameContrasimilarityExponentialAnalyzer)

    registerOperation(new StructureOperation.SigrefBisimilarityAnalyzer)
    registerOperation(new StructureOperation.SigrefWeakBisimilarityAnalyzer)

    registerOperation(new StructureOperation.NaiveTransitiveTauClosure)
    registerOperation(new StructureOperation.NaiveReflexiveTauClosure)

    registerOperation(new StructureOperation.WeakStepRelationClosure)

    registerOperation(new StructureOperation.TauLoopCompressor)
    registerOperation(new StructureOperation.QuotientBuilder)
  }

  def registerOperation(op: StructureOperation) = {
    operations += (op.slug -> op)
    broadcast(Structure.StructureOperationsChanged(operations))
  }

  override def notify(c: ModelComponent.Change) = c match {
    case Source.SourceChange(source, ast) =>
      val beginInterpret = Date.now
      val interpretationResult =
        new Interpreter(ast, NodeID(_), Structure.arrowAnnotator, Structure.nodeAnnotator)
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

  def setReplay(replay: List[() => AlgorithmLogging.LogEntry[NodeID, Structure.ActionLabel, Structure.NodeLabel]]): Unit = {
    currentReplay = replay
    currentReplayStep = 0
    broadcast(Structure.StructureReplayChange(currentReplay))
  }

  def doReplayStep(): Boolean = {
    if (currentReplayStep < currentReplay.length) {
      currentReplay(currentReplayStep)() match {
        case AlgorithmLogging.LogRelation(rel, comment) =>
          val labeled = new LabeledRelation(rel.tupleSet.map {case (p1, p2) => (p1, "", p2)})
          broadcast(Structure.StructureCommentChange(comment))
          broadcast(Structure.StructureRelationChange(labeled))
      }
      currentReplayStep += 1
      true
    } else if (currentReplayStep == currentReplay.length) {
      // reset everything
      broadcast(Structure.StructureRelationChange(relation))
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

  case class StructureCommentChange(comment: String) extends ModelComponent.Change

  case class StructureReplayChange(replay: List[() => AlgorithmLogging.LogEntry[NodeID, Structure.ActionLabel, Structure.NodeLabel]])
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
            act = Symbol(name)
        )
        Interpreting.Success(l)
      } catch {
        case e: Exception => Interpreting.Problem(e.toString(), List(aL))
      }
    case None =>
      Interpreting.Success(emptyActionLabel)
  }


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

  case class StructureDoReplayStep(goToStep: Int = -1) extends StructureAction {
    override def implementStructure(structure: Structure) = {
      if (goToStep >= 0) structure.currentReplayStep = goToStep
      structure.doReplayStep()
    }
  }

}