package io.equiv.eqfiddle.tool.control

import io.equiv.eqfiddle.tool.arch.Action
import io.equiv.eqfiddle.tool.arch.Control
import io.equiv.eqfiddle.tool.model.NodeID
import io.equiv.eqfiddle.ccs.Syntax

class Pipeline(val main: Control) extends ModelComponent {
  
  var pipelineSource = Array[String]()
  var operations = Map[String, StructureOperation]()
  var currentStep = -1
  var operationLines = List[Pipeline.LineInfo]()
  val supportedOperations = Map(
    "compare" -> "Compare two processes with respect to all strong equivalences",
    "compareSilent" -> "Compare two processes with respect to all weak equivalences",
    "minimize" -> "Explore quotient minimizations with respect to all strong equivalences",
    "characterize" -> "What is special about a process when compared with the others"
  )

  def changePipeline(code: String) = {
    pipelineSource = code.split("\n")
  }
  
  def runPipeline() = {
  }
  
  def stepPipeline() = {
    println("pipeline step")
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
  
  def runMetaRunner(meta: String, info: String, line: Int): Boolean = meta match {
    case "compare" =>
      val states = info.split(",").map(_.trim())
      if (states.length == 2) {
        broadcast(Pipeline.PipelineStatusChange(operationLines ++ List(Pipeline.CurrentLine(line))))
        main.dispatchAction(Structure.StructureExamineEquivalences(NodeID(states(0)), NodeID(states(1))))
        true
      } else {
        false
      }
    case "compareSilent" =>
      val states = info.split(",").map(_.trim())
      if (states.length == 2) {
        broadcast(Pipeline.PipelineStatusChange(operationLines ++ List(Pipeline.CurrentLine(line))))
        main.dispatchAction(Structure.StructureExamineEquivalences(NodeID(states(0)), NodeID(states(1)), silentSpectrum = true))
        true
      } else {
        false
      }
    case "minimize" =>
      broadcast(Pipeline.PipelineStatusChange(operationLines ++ List(Pipeline.CurrentLine(line))))
      main.dispatchAction(Structure.StructureMinimize())
      true
    case "characterize" =>
      broadcast(Pipeline.PipelineStatusChange(operationLines ++ List(Pipeline.CurrentLine(line))))
      main.dispatchAction(Structure.StructureCharacterize(NodeID(info.trim())))
      true
    case _ =>
      false
  }


  def notify(change: ModelComponent.Change) = change match {
    case Structure.StructureOperationsChanged(ops) =>
      operations = ops toMap
    case Structure.StructureChange(_) =>
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
      println("pipeline: "+code)
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
  
  case class RunMetaRunner(meta: String, info: String, line: Int) extends PipelineAction {
    override def implementPipeline(pipeline: Pipeline) = {
      pipeline.runMetaRunner(meta, info, line)
    }
  }
}