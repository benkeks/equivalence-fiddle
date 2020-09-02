package de.bbisping.coupledsim.tool.view

import de.bbisping.coupledsim.tool.arch.Control
import de.bbisping.coupledsim.tool.control.ModelComponent

trait ViewComponent {
  val main: Control
  
  def notify(change: ModelComponent.Change)
  
  final def triggerAction = main.dispatchAction(_)
  
  main.registerViewComponent(this)
}