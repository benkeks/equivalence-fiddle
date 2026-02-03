package io.equiv.eqfiddle.tool.view

import d3v4._
import scala.scalajs.js
import scala.scalajs.js.annotation.JSImport
import scala.collection.mutable.HashMap
import org.scalajs.dom
import org.scalajs.dom.EventTarget

/**
 * GraphEditing manages the interplay of HTML-Events, node selection, scrolling/zooming
 * and graph editing behaviors.
 */
object GraphEditing {
  @JSImport("d3-brush", JSImport.Namespace)
  @js.native object D3BrushModule extends js.Object
}

trait GraphEditing extends ViewComponent {
  self: GraphView =>
    
  import GraphView._
      
  val graphEditor = d3.select("#es-graph-editor")
  
  val svg = d3.select("#es-graph")
  
  // these scales ensure that zoom behavior and box selection behavior by sharing them use the same view port
  val viewportX = d3.scaleLinear().domain(js.Array(0.0, 1000.0)).range(js.Array(0.0, 1000.0))
  val viewportY = d3.scaleLinear().domain(js.Array(0.0, 1000.0)).range(js.Array(0.0, 1000.0))
   
  val brush = GraphEditing.D3BrushModule.asInstanceOf[js.Dynamic].brush()
    .on("start", (_: Any) => onSelectionBrushStart())
    .on("brush", (_: Any) => onSelectionBrush())
    .on("end", (_: Any) => onSelectionBrushEnd())
  
  val brushRect = svg.append("g")
    .classed("brush", true)
    
  val zoomWindow = d3.zoom[dom.EventTarget]()
  zoomWindow
    .on("zoom", () => onZoom())

  svg.call(zoomWindow)
    .on("click", () => onClickBackground())
  
  val drag = d3.drag[GraphNode]()
    .on("start", (d: GraphNode) => onDragStart(d))
    .on("drag", (d: GraphNode) => onDrag(d))
    .on("end", (d: GraphNode) => onDragEnd(d))
    
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
  
  def onDrag(node: GraphNode): Unit = {
    editingBehavior.onDrag(node)
  }
  
  def onDragEnd(node: GraphNode): Unit = {
    editingBehavior.onDragEnd(node)
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
  
  def onSelectionBrushStart(): Unit = {
    nodes.foreach { n =>
      n.previouslySelected = n.selected
    }
  }
  
  def onSelectionBrush(): Unit = {
    val selection = d3.event.asInstanceOf[js.Dynamic].selection
    if (js.isUndefined(selection) || selection == null) {
      return
    }
    val ext = selection.asInstanceOf[js.Array[js.Array[Double]]]
    nodes.foreach { node: GraphNode =>
      val inRect =
        node.x.get >= ext(0)(0) && node.x.get <= ext(1)(0) &&
        node.y.get >= ext(0)(1) && node.y.get <= ext(1)(1) &&
        dummyNode != node
      node.selected =
        if (selectionExtensionActive)
          inRect ^ node.previouslySelected
        else
          inRect
      node.selected
    }
    onSelectionChange()
    editingBehavior.onSelectionChange()
  }
  
  def onSelectionBrushEnd(): Unit = {
    brushRect.call(brush.asInstanceOf[js.Dynamic].move.asInstanceOf[js.Function], null)
  }
  
  def setSelectionExtension(active: Boolean) = {
    selectionExtensionActive = active
    if (selectionExtensionActive) {
      svg.call(zoomWindow)
        .on("mousedown.zoom", () => {})
        .on("touchstart.zoom", () => {})
        .on("touchmove.zoom", () => {})
        .on("touchend.zoom", () => {})
      brushRect.select(".background").style("cursor", "crosshair")
      brushRect.call(brush.asInstanceOf[js.Function])
    } else {
      brushRect.call(brush.asInstanceOf[js.Function])
        .on("mousedown.brush", null.asInstanceOf[js.Function1[org.scalajs.dom.EventTarget, Unit]])
        .on("touchstart.brush", null.asInstanceOf[js.Function1[org.scalajs.dom.EventTarget, Unit]])
        .on("touchmove.brush", null.asInstanceOf[js.Function1[org.scalajs.dom.EventTarget, Unit]])
        .on("touchend.brush", null.asInstanceOf[js.Function1[org.scalajs.dom.EventTarget, Unit]])
      brushRect.select(".background").style("cursor", "auto")
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
   
    sceneRoot.attr("transform", d3.event.asInstanceOf[d3.ZoomEvent].transform.toString())
  }
}