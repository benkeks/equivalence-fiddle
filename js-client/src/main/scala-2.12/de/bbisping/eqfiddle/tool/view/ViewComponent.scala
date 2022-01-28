package de.bbisping.eqfiddle.tool.view

import de.bbisping.eqfiddle.tool.arch.Control
import de.bbisping.eqfiddle.tool.control.ModelComponent

trait ViewComponent {
  val main: Control
  
  def notify(change: ModelComponent.Change)
  
  final def triggerAction = main.dispatchAction(_)
  
  main.registerViewComponent(this)
}