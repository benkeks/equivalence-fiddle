package io.equiv.eqfiddle.tool.view

import scala.scalajs.js
import scala.collection.mutable.HashMap
import org.scalajs.dom
import org.scalajs.dom.raw.EventTarget
import d3v4._

/**
 * GraphEditing manages the interplay of HTML-Events, node selection, scrolling/zooming
 * and graph editing behaviors.
 */
trait GraphEditing extends ViewComponent {
  self: GraphView =>
    
  import GraphView._
      
  val graphEditor = d3.select("#es-graph-editor")
  
  val svg = d3.select("#es-graph")
  
  // these scales ensure that zoom behavior and box selection behavior by sharing them use the same view port
  val viewportX = d3.scaleLinear().domain(js.Array(0, 1000)).range(js.Array(0, 1000))
  val viewportY = d3.scaleLinear().domain(js.Array(0, 1000)).range(js.Array(0, 1000))
   
  // val brush = d3.svg.brush().asInstanceOf[Brush[EventTarget]]
  //   .x(viewportX)
  //   .y(viewportY)
  //   .on("brushstart", onSelectionBrushStart _)
  //   .on("brush", onSelectionBrush _)
  //   .on("brushend", onSelectionBrushEnd _)
  
  // val brushRect = svg.append("g")
  //   .classed("brush", true)
    
  val zoomWindow = d3zoom[EventTarget]()
  zoomWindow
    .x(viewportX)
    .y(viewportY)
    .on("zoom", onZoom _)
    
  svg.call(zoomWindow)
    .on("click", onClickBackground _)
  
  val drag = d3drag[GraphNode]()
    .origin((d: GraphNode, _: Double) => d.asInstanceOf[js.Any])
    .on("dragstart", onDragStart _)
    .on("drag", onDrag _)
    .on("dragend", onDragEnd _)
    
  val dragLink = d3drag[NodeLink]()
    .origin((d: NodeLink, _: Double) => d.asInstanceOf[js.Any])
    .on("dragstart", onDragStart _)
    .on("drag", onDrag _)
    .on("dragend", onDragEnd _)
    
  val sceneRoot = svg.append("g")
    
  val behaviors = HashMap[String, GraphEditBehavior]()
  
  var selectionExtensionActive = false
  
  var editingBehavior: GraphEditBehavior = new Object() with GraphEditBehavior
  
  d3.select("body")
    .on("keydown", onKeyDown _)
    .on("keyup", onKeyUp _) 
  
  def registerEditingBehavior(name: String, behavior: GraphEditBehavior) = {
    behaviors.+= (name -> behavior)
  }
  
  def setEditingBehavior(behaviorName: String) = {
    if (editingBehavior != null) {
      editingBehavior.deactivate()
    }
    behaviors.foreach { case (n: String, b: GraphEditBehavior) =>
      d3.select("#"+n).classed("active", behaviorName == n)
    }
    editingBehavior = behaviors(behaviorName)
    editingBehavior.activate()
  }
  
  def selectNode(node: GraphNode) {
    nodes foreach { 
       n: GraphNode =>
        n.selected = 
          if (selectionExtensionActive)
            n.selected != (n == node)
          else
            n == node
    }
    onSelectionChange()
    editingBehavior.onSelectionChange()
  }
  
  def onSelectionChange()
  
  def getSelectedNodes() = {
    nodes.filter(_.selected).toList
  }
  
  def onDragStart(node: GraphNode, id: Double) {
    if (!node.selected) {
      selectNode(node)
    }
    editingBehavior.onDragStart(node)
    
    d3.event.asInstanceOf[js.Dynamic].sourceEvent.asInstanceOf[org.scalajs.dom.DragEvent].stopPropagation()
  }
  
  def onDragStart(link: NodeLink, id: Double) {
    editingBehavior.onDragStart(link)
    js.Object.getOwnPropertyDescriptor(d3.event.asInstanceOf[js.Object],"sourceEvent").value.asInstanceOf[org.scalajs.dom.DragEvent].stopPropagation()
  }
  
  def onDrag(node: GraphNode, id: Double) {
    editingBehavior.onDrag(node)
  }
  
  def onDrag(link: NodeLink, id: Double) {
    editingBehavior.onDrag(link)
  }
  
  def onDragEnd(node: GraphNode, id: Double) {
    editingBehavior.onDragEnd(node)
  }
  
  def onDragEnd(link: NodeLink, id: Double) {
    editingBehavior.onDragEnd(link)
  }
  
  def onHover(node: GraphNode) {
    editingBehavior.onHover(node)
  }
  
  def onHoverEnd(node: GraphNode) {
    editingBehavior.onHoverEnd(node)
  }
  
  def onHover(link: NodeLink) {
    editingBehavior.onHover(link)
  }
  
  def onHoverEnd(link: NodeLink) {
    editingBehavior.onHoverEnd(link)
  }
  
  def onClick(node: GraphNode) {
    editingBehavior.onClick(node)
    d3.event.asInstanceOf[dom.Event].stopPropagation()
  }
  
  def onClickBackground(node: EventTarget) {
    //Note: for reasons I don't understand, node is always undefined...
    editingBehavior.onClick(d3.mouse(sceneRoot.node()))
  }
  
  def onKeyDown(et: EventTarget) {
    if (d3.event.asInstanceOf[org.scalajs.dom.KeyboardEvent].shiftKey && !selectionExtensionActive) {
      setSelectionExtension(true)
    }
  }
  
  def onKeyUp(et: EventTarget) {
    if (selectionExtensionActive && !d3.event.asInstanceOf[org.scalajs.dom.KeyboardEvent].shiftKey) {
      setSelectionExtension(false)
    }
  }
  
  def onSelectionBrushStart(et: Any, id: Double) {
    nodes.foreach { n =>
      n.previouslySelected = n.selected
    }
  }
  
  def onSelectionBrush(et: Any, id: Double) {
    // val ext = brush.extent().asInstanceOf[js.Array[js.Array[Double]]]
    // nodes.foreach { node: GraphNode =>
    //   node.selected = ((node.x.get >= ext(0)(0) && node.x.get <= ext(1)(0) &&
    //                     node.y.get >= ext(0)(1) && node.y.get <= ext(1)(1))
    //                 ^  (node.previouslySelected && selectionExtensionActive))
    //   node.selected
    // }
    // onSelectionChange()
    // editingBehavior.onSelectionChange()
  }
  
  def onSelectionBrushEnd(et: EventTarget, id: Double) {
    // brush.clear()
    // brushRect.call(brush) // something like this is done in http://bl.ocks.org/pkerpedjiev/0389e39fad95e1cf29ce
    //     // it ensures that the brush stays usable when the shift key is released. but it introduces the bug
    //     // that in such situations, subsequent zoom-scrolling also activates the selection brush.
  }
  
  def setSelectionExtension(active: Boolean) = {
    // selectionExtensionActive = active
    // if (selectionExtensionActive) {
    //   svg.call(zoomWindow)
    //     .on("mousedown.zoom", null)
    //     .on("touchstart.zoom", null)                                                                      
    //     .on("touchmove.zoom", null)                                                                       
    //     .on("touchend.zoom", null)
    //   brushRect.select(".background").style("cursor", "crosshair")
    //   brushRect.call(brush)
    // } else {
    //   brushRect.call(brush)
    //    .on("mousedown.brush", null)
    //    .on("touchstart.brush", null)                                                                      
    //    .on("touchmove.brush", null)                                                                       
    //    .on("touchend.brush", null)
    //   brushRect.select(".background").style("cursor", "auto")
    //   svg.call(zoomWindow)
    // }
  }
  
  def deselectAll() {
    nodes.foreach { node: GraphNode =>
      node.selected = false
    }
    onSelectionChange()
    editingBehavior.onSelectionChange()
  }
  
  def onZoom(node: EventTarget, id: Double) {
    
    // deselect all nodes if clicking on background without shift key
    if (!selectionExtensionActive) {
      deselectAll()
    }
    
    val transl = zoomWindow.translate()
    val scale = zoomWindow.scale()
    
    sceneRoot.attr("transform",
      "translate(" + transl._1 + ","
                   + transl._2 + ")"
        + "scale(" + scale + ")")
  }
}