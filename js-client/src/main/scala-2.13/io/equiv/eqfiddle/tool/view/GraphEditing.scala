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
   
  // Manual selection rectangle state
  var selectionRectActive = false
  var selectionStartX = 0.0
  var selectionStartY = 0.0
  var selectionCurrentX = 0.0
  var selectionCurrentY = 0.0
    
  val zoomWindow = d3.zoom[dom.EventTarget]()
  zoomWindow
    .on("zoom", () => onZoom())

  svg.call(zoomWindow)
    .on("mousedown", () => onClickBackground())
    .on("mousemove", () => onMouseMove())
    .on("mouseup", () => onEndSelection())

  val drag = d3.drag[GraphNode]()
    .on("start", (d: GraphNode) => onDragStart(d))
    .on("drag", (d: GraphNode) => onDrag(d))
    .on("end", (d: GraphNode) => onDragEnd(d))
    
  val sceneRoot = svg.append("g")
  
  val selectionRect = sceneRoot.append("rect")
    .classed("brush-selection", true)
    .style("display", "none")

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
    if (d3.event.asInstanceOf[org.scalajs.dom.MouseEvent].button != 0) return
    editingBehavior.onClick(node)
    d3.event.asInstanceOf[dom.Event].stopPropagation()
  }
  
  def onClickBackground(): Unit = {
    if (d3.event.asInstanceOf[org.scalajs.dom.MouseEvent].button != 0) return
    val mouseCoords = d3.mouse(sceneRoot.node())
    val x = mouseCoords(0)
    val y = mouseCoords(1)
    
    // Start selection rectangle if shift is pressed
    if (selectionExtensionActive) {
      selectionRectActive = true
      selectionStartX = x
      selectionStartY = y
      selectionCurrentX = x
      selectionCurrentY = y
      
      // Save previous selection state
      nodes.foreach { n =>
        n.previouslySelected = n.selected
      }
      
      updateSelectionRect()
      d3.event.asInstanceOf[dom.Event].stopPropagation()
    } else {
      deselectAll()
      editingBehavior.onClick((x, y))
    }
  }
  
  def onMouseMove(): Unit = {
    if (selectionRectActive) {
      val mouseCoords = d3.mouse(sceneRoot.node())
      selectionCurrentX = mouseCoords(0)
      selectionCurrentY = mouseCoords(1)
      updateSelectionRect()
      updateNodeSelection()
    }
  }
  
  def onEndSelection(): Unit = {
    selectionRectActive = false
    selectionRect.style("display", "none")
  }
  
  def updateSelectionRect(): Unit = {
    val x = Math.min(selectionStartX, selectionCurrentX)
    val y = Math.min(selectionStartY, selectionCurrentY)
    val width = Math.abs(selectionCurrentX - selectionStartX)
    val height = Math.abs(selectionCurrentY - selectionStartY)
    
    selectionRect
      .attr("x", x)
      .attr("y", y)
      .attr("width", width)
      .attr("height", height)
      .style("display", if (width > 2 || height > 2) "block" else "none")
  }
  
  def updateNodeSelection(): Unit = {
    val minX = Math.min(selectionStartX, selectionCurrentX)
    val maxX = Math.max(selectionStartX, selectionCurrentX)
    val minY = Math.min(selectionStartY, selectionCurrentY)
    val maxY = Math.max(selectionStartY, selectionCurrentY)
    
    nodes.foreach { node: GraphNode =>
      val inRect =
        node.x.get >= minX && node.x.get <= maxX &&
        node.y.get >= minY && node.y.get <= maxY &&
        dummyNode != node
      node.selected =
        if (selectionExtensionActive)
          inRect ^ node.previouslySelected
        else
          inRect
    }
    onSelectionChange()
    editingBehavior.onSelectionChange()
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
  
  def setSelectionExtension(active: Boolean) = {
    selectionExtensionActive = active
    if (selectionExtensionActive) {
      svg.call(zoomWindow)
        .on("mousedown.zoom", () => {})
        .on("touchstart.zoom", () => {})
        .on("touchmove.zoom", () => {})
        .on("touchend.zoom", () => {})
      svg.style("cursor", "crosshair")
    } else {
      svg.call(zoomWindow)
      svg.style("cursor", "auto")
    }
    onEndSelection()
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
    sceneRoot.attr("transform", d3.event.asInstanceOf[d3.ZoomEvent].transform.toString())
  }
}