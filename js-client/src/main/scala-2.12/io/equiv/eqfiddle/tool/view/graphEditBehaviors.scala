package io.equiv.eqfiddle.tool.view

import scala.Left
import scala.Right
import scala.scalajs.js
import scala.scalajs.js.Any.jsArrayOps
import scala.scalajs.js.UndefOr.any2undefOrA
import scala.scalajs.js.UndefOr.undefOr2ops
import scala.scalajs.js.|.from
import org.scalajs.dom.raw.HTMLInputElement
import org.singlespaced.d3js.Ops.fromFunction1To3
import org.singlespaced.d3js.Ops.fromFunction2To3StringPrimitive
import org.singlespaced.d3js.Selection
import org.singlespaced.d3js.d3
import io.equiv.eqfiddle.tool.control.Source
import io.equiv.eqfiddle.tool.control.Structure
import io.equiv.eqfiddle.tool.view.GraphView.NodeLink
import io.equiv.eqfiddle.tool.view.GraphView.GraphNode
import io.equiv.eqfiddle.tool.model.NodeID

trait GraphEditBehavior {
  
  def activate() {}
  
  def deactivate() {}
  
  def onClick(coords: (Double, Double)) {}
  def onClick(node: GraphNode) {}
  
  def onHover(node: GraphNode) {}
  def onHover(node: NodeLink) {}
  
  def onHoverEnd(node: GraphNode) {}
  def onHoverEnd(node: NodeLink) {}
  
  def onDragStart(node: GraphNode) {}
  def onDragStart(link: NodeLink) {}
  
  def onDrag(node: GraphNode) {}
  def onDrag(link: NodeLink) {}
  
  def onDragEnd(node: GraphNode) {}
  def onDragEnd(link: NodeLink) {}
  
  def onSelectionChange() {}
}

class GraphMoveNode(renderer: GraphEditing) extends GraphEditBehavior {
  
  var affectedNodes = List[GraphNode]()
  var initialXY = (0.0, 0.0)
  
  override def onDragStart(node: GraphNode) {
    affectedNodes = renderer.getSelectedNodes()
    affectedNodes.foreach { n => n.fixed = 2 }
  }
  
  override def onDrag(node: GraphNode) {
    val x = js.Object.getOwnPropertyDescriptor(d3.event.asInstanceOf[js.Object],"dx").value.asInstanceOf[Double]
    val y = js.Object.getOwnPropertyDescriptor(d3.event.asInstanceOf[js.Object],"dy").value.asInstanceOf[Double]
    
    affectedNodes.map { n =>
      n.px = n.px.getOrElse(0.0) + x
      n.py = n.py.getOrElse(0.0) + y
    }
  }
  
  override def onDragEnd(node: GraphNode) {
    val updates = affectedNodes.map { n =>
      n.fixed = 1
      (n.nameId.name, Structure.NodeLabel(node.meta.act - Symbol("implicit-main"), n.px.toOption, n.py.toOption))
    }
    renderer.triggerAction(Source.UpdateNodeAnnotationAttributes(updates))
  }
}

class GraphEditNode(renderer: GraphEditing) extends GraphEditBehavior {
  
  val nameInput = d3.select("#es-node-name")
  
  val inputElem = nameInput.node.asInstanceOf[HTMLInputElement]
  
  var activeNode: Option[GraphNode] = None
  var newNode: Option[GraphNode] = None
  
  override def activate() {
    activeNode = None
    newNode = None
    nameInput.classed("active", activeNode.isDefined)
  }
  
  override def deactivate() {
    activeNode = None
    newNode = None
    nameInput.classed("active", activeNode.isDefined)
  }
  
  override def onClick(coords: (Double, Double)) {
    if (activeNode.isDefined) {
      commitName()
      activeNode = None
      newNode = None
    } else {
      val (x,y) = coords
      val node = new GraphNode(NodeID("newEvent"), Structure.emptyLabel)
      node.x = x
      node.px = x
      node.y = y
      node.py = y
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
  
  override def onDragStart(node: GraphNode) {
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
  
  def commitName() {
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
  
  override def onSelectionChange() {
    val selectedNodes = renderer.getSelectedNodes()
    
    if (selectedNodes.length == 2) {
      val names = selectedNodes.map(_.nameId)
      renderer.triggerAction(Structure.StructureExamineEquivalences(names(0), names(1)))
    }
  }
}