package io.equiv.eqfiddle.tool.arch

import scala.collection.mutable.Queue
import io.equiv.eqfiddle.tool.view.ViewComponent
import io.equiv.eqfiddle.tool.control.ModelComponent
import io.equiv.eqfiddle.algo.AlgorithmLogging

trait Control extends ActionDispatcher {
  
  val pendingChanges = Queue[ModelComponent.Change]()
  var changeDeliveryRunning: Boolean = false
  
  /**
   * mutable list of registered view components
   */
  val viewComponents = Queue[ViewComponent]()
  
  /**
   * mutable list of registered model components
   */
  val modelComponents = Queue[ModelComponent]()
  
  def broadcastChange(change: ModelComponent.Change) {
    pendingChanges.enqueue(change)
    
    if (!changeDeliveryRunning) {
      changeDeliveryRunning = true
      while(pendingChanges.nonEmpty) {
        val ch = pendingChanges.dequeue()
        AlgorithmLogging.debugLog("processing " + ch)
        modelComponents.foreach(_.notify(ch))
        viewComponents.foreach(_.notify(ch))
      }
      changeDeliveryRunning = false
    }
  }
  
  def registerViewComponent(component: ViewComponent) {
    viewComponents.enqueue(component)
  }
  
  def registerModelComponent(component: ModelComponent) {
    modelComponents.enqueue(component)
  }
}