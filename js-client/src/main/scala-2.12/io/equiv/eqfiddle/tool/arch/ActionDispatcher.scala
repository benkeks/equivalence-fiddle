package io.equiv.eqfiddle.tool.arch

import org.scalajs.dom
import io.equiv.eqfiddle.tool.control.ModelComponent

trait ActionDispatcher {
  
  final def dispatchAction(action: Action) {
    doAction(action, getActionTarget(action))
  }
  
  final def doAction(a: Action, target: ModelComponent) = {
    try {
      target.implementAction(a)
    } catch {
      case e : Exception => dom.window.alert(e.toString)
    }
  }
  
  def getActionTarget(action: Action): ModelComponent
}