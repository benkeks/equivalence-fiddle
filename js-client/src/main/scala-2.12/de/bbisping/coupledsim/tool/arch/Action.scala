package de.bbisping.coupledsim.tool.arch

import org.scalajs.dom
import de.bbisping.coupledsim.tool.control.ModelComponent

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