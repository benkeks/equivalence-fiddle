package io.equiv.eqfiddle.tool.view

import scala.Left
import scala.Right
import scala.scalajs.js
import scala.scalajs.js.Any.jsArrayOps
import scala.scalajs.js.|.from
import org.scalajs.dom
import org.scalajs.dom.HTMLInputElement
import d3v4._
import io.equiv.eqfiddle.tool.control.Source
import io.equiv.eqfiddle.tool.control.Structure
import io.equiv.eqfiddle.tool.view.GraphView.NodeLink
import io.equiv.eqfiddle.tool.view.GraphView.GraphNode
import io.equiv.eqfiddle.tool.model.NodeID

trait GraphEditBehavior {
  
  def activate(): Unit = {}
  
  def deactivate(): Unit = {}
  
  def onClick(coords: (Double, Double)): Unit = {}
  def onClick(node: GraphNode): Unit = {}
  
  def onHover(node: GraphNode): Unit = {}
  def onHover(node: NodeLink): Unit = {}
  
  def onHoverEnd(node: GraphNode): Unit = {}
  def onHoverEnd(node: NodeLink): Unit = {}
  
  def onDragStart(node: GraphNode): Unit = {}
  def onDragStart(link: NodeLink): Unit = {}
  
  def onDrag(node: GraphNode): Unit = {}
  def onDrag(link: NodeLink): Unit = {}
  
  def onDragEnd(node: GraphNode): Unit = {}
  def onDragEnd(link: NodeLink): Unit = {}
  
  def onSelectionChange(): Unit = {}
}

class GraphMoveNode(renderer: GraphEditing) extends GraphEditBehavior {
  
  var affectedNodes = List[GraphNode]()
  var initialXY = (0.0, 0.0)
  
  override def onDragStart(node: GraphNode): Unit = {
    affectedNodes = renderer.getSelectedNodes()
    affectedNodes.foreach { n => n.fx = n.x; n.fy = n.y }
  }
  
  override def onDrag(node: GraphNode): Unit = {
    val dx = js.Object.getOwnPropertyDescriptor(d3.event.asInstanceOf[js.Object],"dx").value.asInstanceOf[Double]
    val dy = js.Object.getOwnPropertyDescriptor(d3.event.asInstanceOf[js.Object],"dy").value.asInstanceOf[Double]
    
    affectedNodes.map { n =>
      n.x = n.x.getOrElse(0.0) + dx
      n.y = n.y.getOrElse(0.0) + dy
      n.fx = n.x
      n.fy = n.y
    }
  }
  
  override def onDragEnd(node: GraphNode): Unit = {
    val updates = affectedNodes.map { n =>
      n.fx = js.undefined
      n.fy = js.undefined
      (n.nameId.name, Structure.NodeLabel(node.meta.act - Symbol("implicit-main"), n.x.toOption, n.y.toOption))
    }
    renderer.triggerAction(Source.UpdateNodeAnnotationAttributes(updates))
  }
}

class GraphEditNode(renderer: GraphEditing) extends GraphEditBehavior {
  
  val nameInput = d3.select("#es-node-name")
  
val inputElem = nameInput.node().asInstanceOf[HTMLInputElement]
  
  var activeNode: Option[GraphNode] = None
  var newNode: Option[GraphNode] = None
  
  override def activate(): Unit = {
    activeNode = None
    newNode = None
    nameInput.classed("active", activeNode.isDefined)
  }
  
  override def deactivate(): Unit = {
    activeNode = None
    newNode = None
    nameInput.classed("active", activeNode.isDefined)
  }
  
  override def onClick(coords: (Double, Double)): Unit = {
    if (activeNode.isDefined) {
      commitName()
      activeNode = None
      newNode = None
    } else {
      val (x,y) = coords
      val node = new GraphNode(NodeID("newEvent"), Structure.emptyLabel)
      node.x = x
      node.y = y
      newNode = Some(node)
      activeNode = newNode
      nameInput
        .classed("active", activeNode.isDefined)
        .attr("value", node.nameId.name)
        .attr("style", "left: "+node.x+"px; top: "+node.y+"px;")
      inputElem.value = node.nameId.name
      inputElem.select()
      inputElem.focus()
    }
  }
  
  override def onDragStart(node: GraphNode): Unit = {
    if (activeNode.exists(!_.sameRep(node))) {
      commitName()
    }
    newNode = None
    activeNode = Some(node)
    nameInput
      .classed("active", activeNode.isDefined)
      .attr("value", node.nameId.name)
      .attr("style", "left: "+node.x+"px; top: "+node.y+"px;")
    inputElem.value = node.nameId.name
  }
  
  def commitName(): Unit = {
    if (newNode.isDefined) {
      activeNode.foreach { n =>
//        if (inputElem.value != "newEvent") {
//          renderer.triggerAction(Source.UpdateNodeAnnotationAttributes(
//              List((inputElem.value, Structure.EventAnnotation(n.px.toOption, n.py.toOption)))))
//        }
      }
    } else {
      activeNode.foreach { n =>
//        renderer.triggerAction(Source.RenameEvent(
//            n.nameId.name, inputElem.value))
      }
    }
    activeNode = None
    newNode = None
    nameInput.classed("active", activeNode.isDefined)
  }
}

class GraphExamineNodes(renderer: GraphEditing) extends GraphEditBehavior {
  
  override def onSelectionChange(): Unit = {
    val selectedNodes = renderer.getSelectedNodes()
    
    if (selectedNodes.length == 2) {
      val names = selectedNodes.map(_.nameId)
      renderer.triggerAction(Structure.StructureExamineEquivalences(names(0), names(1)))
    }
  }
}