package io.equiv.eqfiddle.ccs

import io.equiv.eqfiddle.tool.model.NodeID
import io.equiv.eqfiddle.ts.WeakTransitionSystem
import io.equiv.eqfiddle.util.{Interpreting, LabeledRelation}

object CCSModel {
  case class ActionLabel(val act: Symbol) {
    def this(name: String) = this(Symbol(name))
    val hash = act.hashCode()
    override def hashCode() = hash
    override def equals(other: Any) = other match {
      case action: ActionLabel => act == action.act
      case _ => false
    }
    def toActString = act.name
    override def toString() = toActString
  }

  case class NodeLabel(
      val act: Set[Symbol],
      val x: Option[Double] = None,
      val y: Option[Double] = None) {
    val hash = act.hashCode()
    override def hashCode() = hash
    override def equals(other: Any) = other match {
      case label: NodeLabel => act == label.act
      case _ => false
    }
    def toStringPairList = {
      act.toList.map(value => (value.name, "")) ++
      x.toList.map(value => ("x", value.round.toString)) ++
      y.toList.map(value => ("y", value.round.toString))
    }
  }

  type TS = WeakTransitionSystem[NodeID, ActionLabel, NodeLabel]

  val emptyLabel = NodeLabel(Set())
  val emptyActionLabel = new ActionLabel("")
  val silentActionLabel = ActionLabel(Symbol("τ"))
  val divergenceActionLabel = ActionLabel(Symbol("δ"))
  val actionChars: Set[Char] = Parser.idChars + '!'

  def nodeAnnotator(nodeDecl: Option[Syntax.NodeAnnotation]): Interpreting.Result[NodeLabel] = nodeDecl match {
    case Some(declaration @ Syntax.NodeAnnotation(_, attributes, _)) =>
      val labels = attributes.collect { case (key, _) if key != "x" && key != "y" => Symbol(key) }
      try {
        Interpreting.Success(NodeLabel(
          labels.toSet,
          attributes.find(_._1 == "x").map(_._2.toDouble),
          attributes.find(_._1 == "y").map(_._2.toDouble)))
      } catch {
        case error: Exception => Interpreting.Problem(error.toString(), List(declaration))
      }
    case None => Interpreting.Success(emptyLabel)
  }

  def arrowAnnotator(arrowLabel: Option[Syntax.Label]): Interpreting.Result[ActionLabel] = arrowLabel match {
    case Some(label @ Syntax.Label(name, _)) =>
      if (name.forall(actionChars)) {
        Interpreting.Success(if (name == "tau" || name == "τ") silentActionLabel else ActionLabel(Symbol(name)))
      } else Interpreting.Problem("Invalid action name: " + name, List(label))
    case None => Interpreting.Success(emptyActionLabel)
  }

  def actionStrIsOutput(action: String): Boolean = action.endsWith("!")
  def actionStrToInput(action: String): String =
    if (actionStrIsOutput(action)) actionStrToInput(action.dropRight(1)) else action
  def actionIsOutput(action: ActionLabel): Boolean = actionStrIsOutput(action.toActString)
  def actionToInput(action: ActionLabel): ActionLabel =
    if (actionIsOutput(action)) ActionLabel(Symbol(actionStrToInput(action.toActString))) else action

  def transitionSystemConstructor(
      relation: LabeledRelation[NodeID, ActionLabel],
      labels: Map[NodeID, NodeLabel]): (Set[NodeID], TS) = {
    val mainNodes = labels.collect { case (id, label) if label.act.contains(Symbol("main")) => id }.toSet
    (mainNodes, new WeakTransitionSystem(relation, labels, Set(silentActionLabel)))
  }

  def interpret(definition: Syntax.Definition, maxStates: Int = 5000): Interpreting.Result[TS] = {
    new Interpreter(definition, NodeID(_), arrowAnnotator, nodeAnnotator,
      actionToInput, actionIsOutput, maxStates = maxStates,
      divergenceMarker = Some(divergenceActionLabel)).result(transitionSystemConstructor)
  }
}