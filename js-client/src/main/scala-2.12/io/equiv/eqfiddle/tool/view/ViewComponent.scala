package io.equiv.eqfiddle.tool.view

import io.equiv.eqfiddle.tool.arch.Control
import io.equiv.eqfiddle.tool.control.ModelComponent

trait ViewComponent {
  val main: Control
  
  def notify(change: ModelComponent.Change)
  
  final def triggerAction = main.dispatchAction(_)
  
  main.registerViewComponent(this)
}