package io.equiv.eqfiddle.tool.control

import scala.collection.mutable.ListBuffer
import io.equiv.eqfiddle.tool.arch.Action
import io.equiv.eqfiddle.tool.arch.Control
import io.equiv.eqfiddle.tool.arch.Undoable
import io.equiv.eqfiddle.algo.AlgorithmLogging


trait ModelComponent {
  val main: Control
  
  val actionLog = ListBuffer[Action]()
  var undoDepth = 0
  
  def implementAction(a: Action) {
    if (a.implement(this) && a.isInstanceOf[Undoable]) {
      if (undoDepth > 0) {
        // delete detached potential redos
        actionLog.remove(0, undoDepth)
        undoDepth = 0
      }
      actionLog prepend a
    }
  }
  
  def undoLastAction(): Boolean = {
    if (actionLog.length > undoDepth) {
      val lastAction = actionLog(undoDepth)
      lastAction match {
        case ua: Undoable =>
          AlgorithmLogging.debugLog("Undo" + ua)
          undoDepth += 1
          return ua.undo()
      }
    }
    false
  }
  
  def redoAction(): Boolean = {
    if (undoDepth > 0) {
      val undineAction = actionLog(undoDepth-1)
      undineAction match {
        case ua: Undoable =>
          AlgorithmLogging.debugLog("Redo" + ua)
          undoDepth -= 1
          return ua.redo()
      }
    }
    false
  }
  
  def clearUndoLog() {
    actionLog.clear()
    undoDepth = 0
  }
  
  def notify(change: ModelComponent.Change)
  
  final def broadcast = main.broadcastChange(_)
  
  main.registerModelComponent(this)
}

object ModelComponent {
  trait Change
}