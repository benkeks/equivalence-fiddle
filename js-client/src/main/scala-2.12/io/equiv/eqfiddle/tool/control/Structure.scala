package io.equiv.eqfiddle.tool.control

import scala.collection.mutable.HashMap

import scala.scalajs.js.Date
import scala.scalajs.js

import io.equiv.eqfiddle.tool.arch.Action
import io.equiv.eqfiddle.tool.arch.Control
import io.equiv.eqfiddle.tool.model.NodeID
import io.equiv.eqfiddle.ccs.Interpreter
import io.equiv.eqfiddle.ccs.Syntax
import io.equiv.eqfiddle.ccs.Parser
import io.equiv.eqfiddle.ts.WeakTransitionSystem
import io.equiv.eqfiddle.util.Coloring
import io.equiv.eqfiddle.util.Interpreting
import io.equiv.eqfiddle.util.Relation
import io.equiv.eqfiddle.util.LabeledRelation
import io.equiv.eqfiddle.algo.AlgorithmLogging
import io.equiv.eqfiddle.spectroscopy.{StrongSpectroscopy, WeakSpectroscopy}
import io.equiv.eqfiddle.hml.StrongObservationNotion
import io.equiv.eqfiddle.hml.WeakObservationNotion
import io.equiv.eqfiddle.hml.Spectrum
import io.equiv.eqfiddle.spectroscopy.Spectroscopy
import io.equiv.eqfiddle.hml.ObservationNotion

class Structure(val main: Control) extends ModelComponent {

  var structure: Structure.TSStructure = null
  var partition: Coloring[NodeID] = Coloring(Map())
  var relation: LabeledRelation[NodeID, String] = LabeledRelation()
  var currentReplayStep: Int = 0
  var currentReplay = List[() => AlgorithmLogging.LogEntry[NodeID]]()

  val operations = HashMap[String, StructureOperation]()

  def init() {

  }

  def registerOperation(op: StructureOperation) = {
    operations += (op.slug -> op)
    broadcast(Structure.StructureOperationsChanged(operations))
  }

  override def notify(c: ModelComponent.Change) = c match {
    case Source.SourceChange(source, ast) =>
      val beginInterpret = Date.now
      val interpretationResult =
        new Interpreter(ast, NodeID(_), Structure.arrowAnnotator, Structure.nodeAnnotator, Structure.actionToInput, Structure.actionIsOutput, divergenceMarker = Some(Structure.divergenceActionLabel))
        .result(Structure.transitionSystemConstructor(_, _))

      interpretationResult match {
        case p: Interpreting.Problem =>
          AlgorithmLogging.debugLog("Interpretation failed after: " + (Date.now - beginInterpret) + "ms.", logLevel = 6)
          broadcast(Structure.StructureChangeFailed(p))
        case Interpreting.Success(is: Structure.TSStructure) =>
          AlgorithmLogging.debugLog("Interpretation took: " + (Date.now - beginInterpret) + "ms.", logLevel = 8)
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
        case AlgorithmLogging.LogSpectrum(spectrum, preords, postords, equations, distCoordsLR, distCoordsRL, comment) =>
          broadcast(Structure.StructureSpectrumChange(spectrum, preords, postords, equations, distCoordsLR, distCoordsRL, comment))
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
    if (structure != null && structure.sameGraphAs(is)) {
      // only a layout change!
      structure = is
      broadcast(Structure.StructureChange(structure, minorChange = true))
    } else {
      structure = is
      broadcast(Structure.StructureChange(structure))
      setPartition(Coloring.fromPartition(Set(is.nodes)))
      setRelation(Relation[NodeID]())
    }
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

  case class StructureChange(tsStructure: TSStructure, minorChange: Boolean = false) extends ModelComponent.Change

  case class StructureChangeFailed(problem: Interpreting.Problem) extends ModelComponent.Change

  case class StructurePartitionChange(partition: Coloring[NodeID]) extends ModelComponent.Change

  case class StructureRelationChange(relation: LabeledRelation[NodeID, String]) extends ModelComponent.Change

  case class StructureRichRelationChange(relation: LabeledRelation[(Set[NodeID], String, Set[NodeID]), String]) extends ModelComponent.Change

  case class StructureCommentChange(comment: String) extends ModelComponent.Change

  case class StructureSpectrumChange[OC <: ObservationNotion](
    spectrum: Spectrum[OC],
    preords: List[String], postords: List[String], equations: List[String],
    distCoordsLR: List[(OC, String)], distCoordsRL: List[(OC, String)],
    comment: String) extends ModelComponent.Change

  case class StructureReplayChange(replay: List[() => AlgorithmLogging.LogEntry[NodeID]])
    extends ModelComponent.Change

  case class StructureOperationsChanged(operations: HashMap[String, StructureOperation]) extends ModelComponent.Change {
    override def toString() = operations.map(_._1).mkString
  }

  case class ActionLabel(
      val act: Symbol) {

    def this(name: String) = {
      this(Symbol(name))
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

  val emptyActionLabel = new ActionLabel("")

  val silentActionLabel = ActionLabel(Symbol("τ"))

  val divergenceActionLabel = ActionLabel(Symbol("δ"))

  def nodeAnnotator(nodeDecl: Option[Syntax.NodeAnnotation]): Interpreting.Result[NodeLabel] = nodeDecl match {
    case Some(nD @ Syntax.NodeAnnotation(name, attribs, pos)) =>
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

  val actionChars: Set[Char] = Parser.idChars + '!'

  def arrowAnnotator(arrowLabel: Option[Syntax.Label]): Interpreting.Result[ActionLabel] = arrowLabel match {
    case Some(aL @ Syntax.Label(name, pos)) =>
      if (name.forall(actionChars)) {
        try {
          val l = if (name == "tau" || name == "τ") silentActionLabel else ActionLabel(Symbol(name))
          Interpreting.Success(l)
        } catch {
          case e: Exception => Interpreting.Problem(e.toString(), List(aL))
        }
      } else {
        Interpreting.Problem("Invalid action name: " + name, List(aL))
      }
    case None =>
      Interpreting.Success(emptyActionLabel)
  }

  def actionIsOutput(a: ActionLabel): Boolean = actionStrIsOutput(a.toActString)
  def actionToInput(a: ActionLabel): ActionLabel =
    if (actionIsOutput(a)) ActionLabel(Symbol(actionStrToInput(a.toActString))) else a

  def actionStrIsOutput(a: String) = a.endsWith("!")
  def actionStrToInput(a: String): String = if (actionStrIsOutput(a)) actionStrToInput(a.dropRight(1)) else a

  def transitionSystemConstructor(
      rel: LabeledRelation[NodeID, ActionLabel],
      labels: Map[NodeID, Structure.NodeLabel],
      oldTs: Option[Structure.TSStructure] = None)
    : (Set[NodeID], WeakTransitionSystem[NodeID,ActionLabel,NodeLabel]) = {

    val silentActions = Set(silentActionLabel)
    val mainNodes = (labels.collect { case (id, label) if label.act.contains('main) => id }).toSet
    (mainNodes, new WeakTransitionSystem(rel, labels, silentActions))
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

  case class StructureExamineEquivalences(n1: NodeID, n2: NodeID, resetReplay: Boolean = true, silentSpectrum: Boolean = false) extends StructureAction {

    override def implementStructure(structure: Structure) = {
      if (resetReplay) {
        structure.setReplay(List())
      }
      
      if (structure.structure.nodes(n1) && structure.structure.nodes(n2)) {

        val begin = Date.now

        val algo = if (silentSpectrum) {
          new WeakSpectroscopy(structure.structure)
        } else {
          new StrongSpectroscopy(structure.structure)
        }
        AlgorithmLogging.uriEncoder = scala.scalajs.js.URIUtils.encodeURI _

        val result = algo.decideAll(List((n1, n2)), Spectroscopy.Config(computeFormulas = true))
        AlgorithmLogging.debugLog("Spectroscopy took: " + (Date.now - begin) + "ms.", logLevel = 7)

        val gameString = result.meta.get("game") match {
          case Some(game) if game != "" =>
            val Some(positionNum) = result.meta.get("game-positions")
            s"""<a href="$game" target="_blank">View game (with $positionNum positions).</a>"""
          case _ => ""
        }

        val leftRightDists = result.foundDistinctionsWithCertificate(n1, n2).map(d => d._1.toString() + d._2.map(_.name).mkString(" (", ",", ")")).mkString("<br>")
        val rightLeftDists = result.foundDistinctionsWithCertificate(n2, n1).map(d => d._1.toString() + d._2.map(_.name).mkString(" (", ",", ")")).mkString("<br>")
        val preords = result.foundPreorders(n1, n2).map(_.name)
        val postords = result.foundPreorders(n2, n1).map(_.name)
        val equations = result.findEqs(n1, n2).map(_.name)
        val distCoordsLR = result.resultFor(n1, n2).flatMap(_.distinctions).map(d => (d._2, d._1.toString()))
        val distCoordsRL = result.resultFor(n2, n1).flatMap(_.distinctions).map(d => (d._2, d._1.toString()))
        val replay = List(
          () => AlgorithmLogging.LogRelation(result.toPreorderingRelation(), s"Preordered by:<div class='preorderings'>${preords.mkString("<br>")}</div>"),
          () => AlgorithmLogging.LogRelation(result.toDistinctionRelation(), s"Left-right-distinguished by:<div class='distinctions'>$leftRightDists</div>"),
          //() => AlgorithmLogging.LogRelation(result.toDistinctionRelation(), s"Right-left-distinguished by:<div class='distinctions'>$rightLeftDists</div>"),
          () => AlgorithmLogging.LogRelation(result.toEquivalencesRelation(), s"Equated by:<div class='equations'>${equations.mkString("<br>")}</div>"),
          () => AlgorithmLogging.LogSpectrum[NodeID, ObservationNotion](result.spectrum, preords, postords, equations, distCoordsLR, distCoordsRL, s"Show spectrum. $gameString")
        )
        structure.setReplay(replay)
        structure.main.doAction(StructureDoReplayStep(), structure)

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

  case class StructureCheckEquivalence(n1: NodeID, n2: NodeID, notion: String, resetReplay: Boolean = true) extends StructureAction {

    override def implementStructure(structure: Structure) = {
      if (resetReplay) {
        structure.setReplay(List())
      }
      
      if (structure.structure.nodes(n1) && structure.structure.nodes(n2)) {

        val begin = Date.now

        val algo = 
          if (WeakObservationNotion.LTBTS.getSpectrumClass.isDefinedAt(notion)) {
            new WeakSpectroscopy(structure.structure)
          } else if (StrongObservationNotion.LTBTS.getSpectrumClass.isDefinedAt(notion)) {
            new StrongSpectroscopy(structure.structure)
          } else {
            throw new Exception(
              s"Notion ‹$notion› is not defined.\n\nPossible names would be:\n ${(
                StrongObservationNotion.LTBTS.getSpectrumClass.keys ++
                WeakObservationNotion.LTBTS.getSpectrumClass.keys).mkString(", ")}")
          }

        AlgorithmLogging.uriEncoder = scala.scalajs.js.URIUtils.encodeURI _

        val result = algo.checkIndividualPreorder(List((n1, n2), (n2, n1)), notion)
        AlgorithmLogging.debugLog("Preorder check took: " + (Date.now - begin) + "ms.", logLevel = 7)

        val Some(lrResult) = result.items.find(r => r.left == n1 && r.right == n2)
        val Some(rlResult) = result.items.find(r => r.left == n2 && r.right == n1)

        val replay = List(
          () => AlgorithmLogging.LogRelation(
            new LabeledRelation[NodeID, String](result.relation),
            {
              if (lrResult.isMaintained && rlResult.isMaintained)
                "States are <strong>equivalent</strong>."
              else if (lrResult.isMaintained)
                "States are strictly <strong>preordered</strong> (only from left to right)."
              else if (rlResult.isMaintained)
                "States are <strong>inversely preordered</strong> (only from right to left)."
              else
                "States are <strong>not</strong> preordered (nor equivalent)"
            } + {
              result.meta.get("game") match {
                case Some(game) if game != "" =>
                  val Some(positionNum) = result.meta.get("game-positions")
                  s"""<p><a href="$game" target="_blank">View game (with $positionNum positions).</a></p>"""
                case _ => ""
              }
            }
          )
        )
        structure.setReplay(replay)
        structure.main.doAction(StructureDoReplayStep(), structure)

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

      val algo = new StrongSpectroscopy(structure.structure)

      val comparedPairs = for {
        n1i <- 0 until states.length
        n2j <- (n1i + 1) until states.length
      } yield (states(n1i), states(n2j))

      val result = algo.decideAll(comparedPairs, Spectroscopy.Config(computeFormulas = false, energyCap = 3))
      AlgorithmLogging.debugLog("Minimization Spectroscopy took: " + (Date.now - begin) + "ms.", logLevel = 7)

      val distRel = result.toDistancesRelation()
      val lubDists = distRel.tupleSet

      val eqLevels =
        distRel.labels.map(result.spectrum.getStrongestPreorderClassFromClass(_))
        .flatten.toSet[Spectrum.EquivalenceNotion[StrongObservationNotion]].toList.sortBy(_.obsNotion.toTuple).reverse

      val replay = for {
        Spectrum.EquivalenceNotion(name, obsNotion) <- eqLevels
        resultRelation = for {
          (p, d, q) <- lubDists
          if !d.exists(_ <= obsNotion)
        } yield (p, "eq", q)
        msg = obsNotion.toTuple.toString().replace(Int.MaxValue.toString(), "∞") + " " + name
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

        val algo = new StrongSpectroscopy(structure.structure)

        val comparedPairs = for {
          n2 <- structure.structure.nodes
        } yield (node, n2)

        val result = algo.decideAll(comparedPairs, Spectroscopy.Config(computeFormulas = false, energyCap = 3))
        AlgorithmLogging.debugLog("Characterization Spectroscopy took: " + (Date.now - begin) + "ms.", logLevel = 7)

        for {
          res <- result.relationItems//.find(r => r.left == n1 && r.right == n2)
          Spectroscopy.ResultItem(_, _, distinctions, preorderings) = res
        } {
          val dists = distinctions.map(d => d._1.toString() + d._3.map(_.name).mkString(" (", ",", ")")).mkString("<br>")
          val preords = preorderings.map(_.name).mkString("<br>")
          val replay = List(
            () => AlgorithmLogging.LogRelation(result.toPreorderingRelation(), s"Preordered by:<div class='preorderings'>$preords</div>"),
            () => AlgorithmLogging.LogRelation(result.toDistinctionRelation(), s"Distinguished by:<div class='distinctions'>$dists</div>"),
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