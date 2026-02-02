package io.equiv.eqfiddle.tool.view

import d3v4._
import scala.scalajs.js
import scala.collection.mutable.HashMap
import org.scalajs.dom
import org.scalajs.dom.EventTarget

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
  val viewportX = d3.scaleLinear().domain(js.Array(0.0, 1000.0)).range(js.Array(0.0, 1000.0))
  val viewportY = d3.scaleLinear().domain(js.Array(0.0, 1000.0)).range(js.Array(0.0, 1000.0))
   
  // Note: D3v4 brush API has changed significantly - extent is now set differently
  // Brush no longer uses .x() and .y() methods
  // val brush = d3.brush()
  //   .on("start", onSelectionBrushStart _)
  //   .on("brush", onSelectionBrush _)
  //   .on("end", onSelectionBrushEnd _)
  
  val brushRect = svg.append("g")
    .classed("brush", true)
    
  val zoomWindow = d3.zoom[dom.EventTarget]()
  zoomWindow
    // Note: D3v4 zoom no longer uses .x() and .y() - it uses transform-based approach
    .on("zoom", () => onZoom())
    
  svg.call(zoomWindow)
    .on("click", () => onClickBackground())
  
  val drag = d3.drag[GraphNode]()
    // Note: D3v4 drag origin API changed - subject replaces origin
    // .subject((d: GraphNode) => d.asInstanceOf[js.Any])
    .on("start", (d: GraphNode) => onDragStart(d))
    .on("drag", (d: GraphNode) => onDrag(d))
    .on("end", (d: GraphNode) => onDragEnd(d))
    
  val dragLink = d3.drag[NodeLink]()
    // Note: D3v4 drag origin API changed
    // .subject((d: NodeLink) => d.asInstanceOf[js.Any])
    .on("start", (d: NodeLink) => onDragStart(d))
    .on("drag", (d: NodeLink) => onDrag(d))
    .on("end", (d: NodeLink) => onDragEnd(d))
    
  val sceneRoot = svg.append("g")
    
  val behaviors = HashMap[String, GraphEditBehavior]()
  
  var selectionExtensionActive = false
  
  var editingBehavior: GraphEditBehavior = new Object() with GraphEditBehavior
  
  d3.select("body")
    .on("keydown", () => onKeyDown())
    .on("keyup", () => onKeyUp()) 
  
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
  
  def selectNode(node: GraphNode): Unit = {
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
  
  def onSelectionChange(): Unit
  
  def getSelectedNodes() = {
    nodes.filter(_.selected).toList
  }

  def onHoverChange(): Unit
  
  def onDragStart(node: GraphNode): Unit = {
    if (!node.selected) {
      selectNode(node)
    }
    editingBehavior.onDragStart(node)
    
    d3.event.asInstanceOf[js.Dynamic].sourceEvent.asInstanceOf[org.scalajs.dom.DragEvent].stopPropagation()
  }
  
  def onDragStart(link: NodeLink): Unit = {
    editingBehavior.onDragStart(link)
    js.Object.getOwnPropertyDescriptor(d3.event.asInstanceOf[js.Object],"sourceEvent").value.asInstanceOf[org.scalajs.dom.DragEvent].stopPropagation()
  }
  
  def onDrag(node: GraphNode): Unit = {
    editingBehavior.onDrag(node)
  }
  
  def onDrag(link: NodeLink): Unit = {
    editingBehavior.onDrag(link)
  }
  
  def onDragEnd(node: GraphNode): Unit = {
    editingBehavior.onDragEnd(node)
  }
  
  def onDragEnd(link: NodeLink): Unit = {
    editingBehavior.onDragEnd(link)
  }
  
  def onHover(node: GraphNode): Unit = {
    node.hovered = true
    editingBehavior.onHover(node)
    onHoverChange()
  }
  
  def onHoverEnd(node: GraphNode): Unit = {
    node.hovered = false
    editingBehavior.onHoverEnd(node)
    onHoverChange()
  }
  
  def onHover(link: NodeLink): Unit = {
    editingBehavior.onHover(link)
  }
  
  def onHoverEnd(link: NodeLink): Unit = {
    editingBehavior.onHoverEnd(link)
  }
  
  def onClick(node: GraphNode): Unit = {
    editingBehavior.onClick(node)
    d3.event.asInstanceOf[dom.Event].stopPropagation()
  }
  
  def onClickBackground(): Unit = {
    //Note: for reasons I don't understand, node is always undefined...
    val mouseCoords = d3.mouse(sceneRoot.node())
    editingBehavior.onClick((mouseCoords(0), mouseCoords(1)))
  }
  
  def onKeyDown(): Unit = {
    if (d3.event.asInstanceOf[org.scalajs.dom.KeyboardEvent].shiftKey && !selectionExtensionActive) {
      setSelectionExtension(true)
    }
  }
  
  def onKeyUp(): Unit = {
    if (selectionExtensionActive && !d3.event.asInstanceOf[org.scalajs.dom.KeyboardEvent].shiftKey) {
      setSelectionExtension(false)
    }
  }
  
  def onSelectionBrushStart(et: Any): Unit = {
    nodes.foreach { n =>
      n.previouslySelected = n.selected
    }
  }
  
  def onSelectionBrush(et: Any): Unit = {
    // Brush API not fully implemented in D3v4 migration yet
    // TODO: Implement rectangular selection with d3.brush() if needed
  }
  
  def onSelectionBrushEnd(et: EventTarget): Unit = {
    // Brush API not fully implemented in D3v4 migration yet
    // TODO: Implement rectangular selection with d3.brush() if needed
  }
  
  def setSelectionExtension(active: Boolean) = {
    selectionExtensionActive = active
    if (selectionExtensionActive) {
      svg.call(zoomWindow)
        .on("mousedown.zoom", () => {})
        .on("touchstart.zoom", () => {})
        .on("touchmove.zoom", () => {})
        .on("touchend.zoom", () => {})
      // brushRect.select(".background").style("cursor", "crosshair")
      // brushRect.call(brush)
    } else {
      // brushRect.call(brush)
       // .on("mousedown.brush", null)
       // .on("touchstart.brush", null)                                                                      
       // .on("touchmove.brush", null)                                                                       
       // .on("touchend.brush", null)
      // brushRect.select(".background").style("cursor", "auto")
      svg.call(zoomWindow)
    }
  }
  
  def deselectAll(): Unit = {
    nodes.foreach { node: GraphNode =>
      node.selected = false
    }
    onSelectionChange()
    onHoverChange()
    editingBehavior.onSelectionChange()
  }
  
  def onZoom(): Unit = {
    
    // deselect all nodes if clicking on background without shift key
    if (!selectionExtensionActive) {
      deselectAll()
    }
    
    // D3v4 zoom uses transform instead of translate/scale methods
    val transform = zoomWindow.asInstanceOf[js.Dynamic].transform.asInstanceOf[js.Dynamic]
    val x = transform.x.asInstanceOf[Double]
    val y = transform.y.asInstanceOf[Double]
    val k = transform.k.asInstanceOf[Double]
    
    sceneRoot.attr("transform",
      "translate(" + x + ","
                   + y + ")"
        + "scale(" + k + ")")
  }
}