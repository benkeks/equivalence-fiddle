package io.equiv.eqfiddle.spectroscopy

import io.equiv.eqfiddle.ccs.{CCSModel, Interpreter, Parser, Syntax}
import io.equiv.eqfiddle.hml.{HML, ObservationNotion}
import io.equiv.eqfiddle.tool.model.NodeID
import io.equiv.eqfiddle.ts.WeakTransitionSystem
import io.equiv.eqfiddle.util.Interpreting

object CCSSpectroscopy {
  case class Request(source: String, left: String, right: String,
      weak: Boolean = false, maxStates: Int = 5000)
  case class Failure(code: String, message: String, line: Int = 0, column: Int = 0)
  case class Analysis(
      transitionSystem: CCSModel.TS,
      result: Spectroscopy.Result[NodeID, CCSModel.ActionLabel, ObservationNotion, HML.Formula[CCSModel.ActionLabel]])
  case class CheckAnalysis(transitionSystem: CCSModel.TS, result: Spectroscopy.IndividualNotionResult[NodeID])

  def parse(source: String): Either[Failure, Syntax.Definition] = {
    val parser = new Parser(source)
    parser.parse match {
      case failure: parser.ParseFail[_] =>
        Left(Failure("PARSE_ERROR", failure.msg, failure.position.line + 1, failure.position.col + 1))
      case parser.ParseSuccess(definition, _) => Right(definition)
    }
  }

  private def interpreted[TS](result: Interpreting.Result[TS]): Either[Failure, TS] = result match {
    case problem: Interpreting.Problem =>
      val position = problem.expr.headOption.map(_.position)
      Left(Failure("INTERPRETATION_ERROR", problem.msg,
        position.map(_.line + 1).getOrElse(0), position.map(_.col + 1).getOrElse(0)))
    case Interpreting.Success(ts) => Right(ts)
  }

  def load(source: String, maxStates: Int = 5000): Either[Failure, CCSModel.TS] = {
    if (maxStates <= 0) Left(Failure("INVALID_REQUEST", "maxStates must be positive."))
    else parse(source).flatMap(definition => interpreted(CCSModel.interpret(definition, maxStates)))
  }

  def loadLTS(source: String): Either[Failure, WeakTransitionSystem[NodeID, String, String]] = {
    parse(source).flatMap(loadLTS)
  }

  def loadLTS(definition: Syntax.Definition): Either[Failure, WeakTransitionSystem[NodeID, String, String]] = {
    interpreted(new Interpreter[NodeID, String, String](definition, NodeID(_),
      label => Interpreting.Success(label.map(_.name).getOrElse("")),
      annotation => Interpreting.Success(annotation.map(_.name).getOrElse("")),
      CCSModel.actionStrToInput, CCSModel.actionStrIsOutput,
      divergenceMarker = Some("δ")).result())
  }

  private def requestedSystem(request: Request): Either[Failure, CCSModel.TS] = {
    load(request.source, request.maxStates).flatMap { ts =>
      if (!ts.nodes(NodeID(request.left)) || !ts.nodes(NodeID(request.right))) {
        val unknown = if (!ts.nodes(NodeID(request.left))) request.left else request.right
        Left(Failure("UNKNOWN_PROCESS", "Unknown process: " + unknown))
      } else Right(ts)
    }
  }

  def run(request: Request): Either[Failure, Analysis] = {
    requestedSystem(request).map { ts =>
      Analysis(ts, Spectroscopy.compare(ts, NodeID(request.left), NodeID(request.right), request.weak,
        Spectroscopy.Config(computeFormulas = true)))
    }
  }

  def check(request: Request, notion: String): Either[Failure, CheckAnalysis] = {
    if (!Spectroscopy.checkNotions(notion)) Left(Failure("UNKNOWN_NOTION", "Unknown notion: " + notion))
    else requestedSystem(request).map { ts =>
      CheckAnalysis(ts, Spectroscopy.check(ts, NodeID(request.left), NodeID(request.right), notion))
    }
  }
}