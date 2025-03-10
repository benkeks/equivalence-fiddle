package io.equiv.eqfiddle.tool.control

import io.equiv.eqfiddle.tool.arch.Action
import io.equiv.eqfiddle.tool.arch.Control
import io.equiv.eqfiddle.tool.model.NodeID
import io.equiv.eqfiddle.ccs.Syntax
import io.equiv.eqfiddle.algo.AlgorithmLogging

class Pipeline(val main: Control) extends ModelComponent {
  
  var pipelineSource = Array[String]()
  var operations = Map[String, StructureOperation]()
  var currentStep = -1
  var operationLines = List[Pipeline.LineInfo]()
  val supportedOperations = Map(
    "compare" -> "Compare two processes with respect to all strong equivalences",
    "compareSilent" -> "Compare two processes with respect to all weak equivalences",
    "check" -> "Compare two processes with respect to a specific equivalence",
    "minimize" -> "Explore quotient minimizations with respect to all strong equivalences",
    "characterize" -> "What is special about a process when compared with the others"
  )

  def changePipeline(code: String) = {
    pipelineSource = code.split("\n")
  }
  
  def runPipeline() = {
  }
  
  def stepPipeline() = {
    AlgorithmLogging.debugLog("pipeline step")
    if (pipelineSource.length <= currentStep) {
      false
    } else {
      val currentLine = pipelineSource(currentStep).trim()
      val operation = operations.get(currentLine)
      
      for (
        op <- operation
      ) {
        main.dispatchAction(Structure.StructureCallOperation(op.slug))
      }
      
      setStep(currentStep + 1)
      
      operation.isDefined
    }
  }
  
  def resetPipeline() = {
    setStep(-1)
  }
  
  def setStep(step: Int) = {
    currentStep = step
    if (currentStep >= 0) {
      broadcast(Pipeline.PipelineStatusChange(operationLines ++ List(Pipeline.CurrentLine(currentStep))))
    } else {
      broadcast(Pipeline.PipelineStatusChange(operationLines))
    }
  }
  
  def runMetaRunner(meta: String, args: List[String], line: Int): Boolean = meta match {
    case "compare" =>
      if (args.length == 2) {
        broadcast(Pipeline.PipelineStatusChange(operationLines ++ List(Pipeline.CurrentLine(line))))
        main.dispatchAction(Structure.StructureExamineEquivalences(NodeID(args(0)), NodeID(args(1))))
        true
      } else {
        throw new Exception("Need two process names as arguments. @compare proc1, proc2")
        false
      }
    case "compareSilent" =>
      if (args.length == 2) {
        broadcast(Pipeline.PipelineStatusChange(operationLines ++ List(Pipeline.CurrentLine(line))))
        main.dispatchAction(Structure.StructureExamineEquivalences(NodeID(args(0)), NodeID(args(1)), silentSpectrum = true))
        true
      } else {
        throw new Exception("Need two process names as arguments. @compareSilent proc1, proc2")
        false
      }
    case "check" =>
      if (args.length == 3) {
        broadcast(Pipeline.PipelineStatusChange(operationLines ++ List(Pipeline.CurrentLine(line))))
        main.dispatchAction(Structure.StructureCheckEquivalence(NodeID(args(1)), NodeID(args(2)), args(0)))
        true
      } else {
        throw new Exception("Need a notion name and two process names as arguments. @check \"notion\", proc1, proc2")
        false
      }
    case "minimize" =>
      if (args.length == 1) {
        broadcast(Pipeline.PipelineStatusChange(operationLines ++ List(Pipeline.CurrentLine(line))))
        main.dispatchAction(Structure.StructureMinimize())
        true
      } else {
        throw new Exception("Need one argument. @minimize \"strong\"")
        false
      }
    case "characterize" =>
      broadcast(Pipeline.PipelineStatusChange(operationLines ++ List(Pipeline.CurrentLine(line))))
      main.dispatchAction(Structure.StructureCharacterize(NodeID(args(0).trim())))
      true
    case _ =>
      false
  }


  def notify(change: ModelComponent.Change) = change match {
    case Structure.StructureOperationsChanged(ops) =>
      operations = ops toMap
    case Structure.StructureChange(_, _) =>
      resetPipeline()
    case Source.SourceChange(_, ast) =>
      operationLines = ast.defs.collect {
        case Syntax.MetaDeclaration(key, value, pos) if supportedOperations.contains(key) =>
          Pipeline.OperationLine(pos.line, supportedOperations(key))
      }
      
    case _ => 
  }
}

object Pipeline {
  
  abstract class LineInfo(line: Int)
  case class CurrentLine(line: Int) extends LineInfo(line)
  case class OperationLine(line: Int, explanation: String) extends LineInfo(line)
  
  case class PipelineStatusChange(pipelineStatus: List[LineInfo]) extends ModelComponent.Change
  
  abstract sealed class PipelineAction extends Action {
    override def implement(target: ModelComponent): Boolean = target match {
      case s: Pipeline => 
        implementPipeline(s)
      case _ =>
        false
    }
    
    def implementPipeline(pipeline: Pipeline): Boolean
  }
  
  case class LoadPipeline(code: String) extends PipelineAction {
    override def implementPipeline(pipeline: Pipeline) = {
      AlgorithmLogging.debugLog("pipeline: "+code)
      pipeline.changePipeline(code)
      true
    }
  }
  
  case class StepPipeline() extends PipelineAction {
    override def implementPipeline(pipeline: Pipeline) = {
      pipeline.stepPipeline()
      true
    }
  }
  
  case class RunPipeline() extends PipelineAction {
    override def implementPipeline(pipeline: Pipeline) = {
      pipeline.runPipeline()
      true
    }
  }
  
  case class ResetPipeline() extends PipelineAction {
    override def implementPipeline(pipeline: Pipeline) = {
      pipeline.resetPipeline()
      true
    }
  }
  
  case class RunMetaRunner(meta: String, info: List[String], line: Int) extends PipelineAction {
    override def implementPipeline(pipeline: Pipeline) = {
      pipeline.runMetaRunner(meta, info, line)
    }
  }
}