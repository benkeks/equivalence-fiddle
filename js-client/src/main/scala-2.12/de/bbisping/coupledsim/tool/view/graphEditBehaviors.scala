package de.bbisping.coupledsim.tool.view

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
import de.bbisping.coupledsim.tool.control.Source
import de.bbisping.coupledsim.tool.control.Structure
import de.bbisping.coupledsim.tool.view.GraphView.NodeLink
import de.bbisping.coupledsim.tool.view.GraphView.GraphNode
import de.bbisping.coupledsim.tool.view.GraphView.LinkTrait
import de.bbisping.coupledsim.tool.model.NodeID

trait GraphEditBehavior {
  
  def activate() {}
  
  def deactivate() {}
  
  def onClick(coords: (Double, Double)) {}
  def onClick(node: GraphNode) {}
  
  def onHover(node: GraphNode) {}
  def onHover(node: LinkTrait) {}
  
  def onHoverEnd(node: GraphNode) {}
  def onHoverEnd(node: LinkTrait) {}
  
  def onDragStart(node: GraphNode) {}
  def onDragStart(link: LinkTrait) {}
  
  def onDrag(node: GraphNode) {}
  def onDrag(link: LinkTrait) {}
  
  def onDragEnd(node: GraphNode) {}
  def onDragEnd(link: LinkTrait) {}
  
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
      (n.nameId.name, Structure.NodeLabel(node.meta.act, n.px.toOption, n.py.toOption))
    }
    renderer.triggerAction(Source.UpdateEventDeclarationAttributes(updates))
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
    if (activeNode.exists(!_.sameNode(node))) {
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
//          renderer.triggerAction(Source.UpdateEventDeclarationAttributes(
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

abstract class GraphConnectNodes(renderer: GraphEditing) extends GraphEditBehavior {
  var selectedTargetNode: GraphNode = null  
  var sourceNode: GraphNode = null
  
  val previewLine = renderer.sceneRoot
    .append("line")
    .classed("es-preview-line", true)
  
  def reset() {
    selectedTargetNode = null
    sourceNode = null
    previewLine.classed("active", false)
    
    val nodeViews: Selection[GraphNode] = renderer.sceneRoot.selectAll(".node")
    nodeViews.classed("connection-possible", false)
  }
  
  override def onHover(node: GraphNode) {
    if (sourceNode != null && !sourceNode.sameNode(selectedTargetNode)) {
      selectedTargetNode = node
    
      val nodeViews: Selection[GraphNode] = renderer.sceneRoot.selectAll(".node")
      nodeViews.classed("connection-possible", (n: GraphNode) => node.sameNode(n))
    }
  }
  
  override def onHoverEnd(node: GraphNode) {
    if (selectedTargetNode != null && selectedTargetNode.sameNode(node)) {
      selectedTargetNode = null
    
      val nodeViews: Selection[GraphNode] = renderer.sceneRoot.selectAll(".node")
      nodeViews.classed("connection-possible", false)
    }
  }
  
  override def onDragStart(node: GraphNode) {
    sourceNode = node
    previewLine.classed("active", true)
    selectedTargetNode = null
  }
  
  override def onDrag(node: GraphNode) {
    var x = js.Object.getOwnPropertyDescriptor(d3.event.asInstanceOf[js.Object],"x").value.asInstanceOf[Double]
    var y = js.Object.getOwnPropertyDescriptor(d3.event.asInstanceOf[js.Object],"y").value.asInstanceOf[Double]
    
    if (sourceNode != null) {
      if (selectedTargetNode != null) {
        x = selectedTargetNode.x.get
        y = selectedTargetNode.y.get
      }
      
      previewLine
        .attr("x1", sourceNode.x.get)
        .attr("y1", sourceNode.y.get)
        .attr("x2", x)
        .attr("y2", y)
    }
  }
  
  override def onDragEnd(node: GraphNode) {
    if (sourceNode != null && selectedTargetNode != null) {
      connectionReaction(sourceNode, selectedTargetNode)
    }
    reset()
  }
  
  def connectionReaction(srcNode: GraphNode, tarNode: GraphNode)
}

class GraphConnectStepTo(renderer: GraphEditing) extends GraphConnectNodes(renderer) {
    previewLine.classed("stepto", true)
    
    def connectionReaction(srcNode: GraphNode, tarNode: GraphNode) {
//      renderer.triggerAction(Source.InsertEnablesConnection(
//        srcNode.esEvent.name.name, tarNode.esEvent.name.name))
    }
}

class GraphDeleteNodeOrLink(renderer: GraphEditing) extends GraphEditBehavior {
  
  var hover: Option[Either[GraphNode, LinkTrait]] = None
  
  private def nodeActive(node: GraphNode) = {
    hover match {
      case Some(Left(n)) => n.sameNode(node)
      case _ => false
    }
  }
  
  private def linkActive(link: NodeLink) = {
    hover match {
      case Some(Right(l)) => l.sameLink(link)
      case _ => false
    }
  }
  
  private def updateView() {
    val nodeViews: Selection[GraphNode] = renderer.sceneRoot.selectAll(".node")
    nodeViews.classed("deletion-possible", nodeActive _)
  }
  
  override def activate() {
    hover = None
    updateView()
  }
  
  override def deactivate() {
    hover = None
    updateView()
  }
  
  override def onHover(n: GraphNode) {
    hover = Some(Left(n))
    updateView()
  }
  
  override def onHoverEnd(n: GraphNode) {
    hover = None
    updateView()
  }
  
  override def onDragStart(node: GraphNode) {
    hover = Some(Left(node))
    commit()
  }
  
  override def onDragStart(link: LinkTrait) {
    hover = Some(Right(link))
    commit()
  }
  
  def commit() {
    hover match {
//      case Some(Left(n)) => 
//        renderer.triggerAction(Source.DeleteEvent(n.esEvent.name.name))
//      case Some(Right(l: EventLink)) =>
//        renderer.triggerAction(Source.DeleteRelation(l.source.esEvent.name.name, l.target.esEvent.name.name))
//      case Some(Right(r: LinkLink)) if r.savedRule.nonEmpty =>
//        renderer.triggerAction(Source.DeleteHORelation(r.savedRule.get))
      case _ =>
        //false
    }
    hover = None
    updateView()
  }
}

class GraphExamineNodes(renderer: GraphEditing) extends GraphEditBehavior {
  
  override def onSelectionChange() {
    val selectedNodes = renderer.getSelectedNodes()
    
    if (selectedNodes.length == 2) {
      val names = selectedNodes.map(_.nameId)
      Console.out.println(names.mkString(","))
      renderer.triggerAction(Structure.StructureExamineEquivalences(names(0), names(1)))
    }
  }
}