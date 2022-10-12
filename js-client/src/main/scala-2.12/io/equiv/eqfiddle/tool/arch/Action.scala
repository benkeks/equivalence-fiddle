package io.equiv.eqfiddle.tool.arch

import org.scalajs.dom
import io.equiv.eqfiddle.tool.control.ModelComponent

abstract class Action {
  def implement(target: ModelComponent): Boolean
}

trait Undoable {
  var undo: () => Boolean
  var redo: () => Boolean
}

trait ActionUndo extends Action {
  override def implement(target: ModelComponent) = {
    target.undoLastAction()
  }
}

trait ActionRedo extends Action {
  override def implement(target: ModelComponent) = {
    target.redoAction()
  }
}