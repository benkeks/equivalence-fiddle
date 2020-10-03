package de.bbisping.coupledsim.tool.view

import scala.annotation.migration
import scala.scalajs.js.Any.fromFunction1
import scala.scalajs.js.Any.fromFunction2
import scala.scalajs.js.Any.jsArrayOps
import scala.scalajs.js.Any.wrapArray
import scala.scalajs.js.Tuple2.fromScalaTuple2
import scala.scalajs.js.UndefOr.undefOr2ops
import scala.scalajs.js.|.from
import org.scalajs.dom
import org.scalajs.dom.raw.EventTarget
import org.scalajs.dom.raw.HTMLInputElement
import org.singlespaced.d3js.Ops.asPrimitive
import org.singlespaced.d3js.Ops.fromFunction1To2
import org.singlespaced.d3js.Ops.fromFunction1To3
import org.singlespaced.d3js.Ops.fromFunction2To3
import org.singlespaced.d3js.Ops.fromFunction2To3DoublePrimitive
import org.singlespaced.d3js.Ops.fromFunction2To3StringPrimitive
import org.singlespaced.d3js.Selection
import org.singlespaced.d3js.d3
import de.bbisping.coupledsim.tool.arch.Control
import de.bbisping.coupledsim.tool.control.ModelComponent
import de.bbisping.coupledsim.tool.control.Structure
import de.bbisping.coupledsim.util.Partition
import de.bbisping.coupledsim.tool.model.NodeID
import de.bbisping.coupledsim.util.Coloring
import de.bbisping.coupledsim.util.Relation
import de.bbisping.coupledsim.util.LabeledRelation

class GraphRenderer(val main: Control)
  extends GraphView
  with GraphEditing
  with ViewComponent {
  
  import GraphView._
  
  registerEditingBehavior("es-graph-move", new GraphMoveNode(this))
  registerEditingBehavior("es-graph-rename", new GraphEditNode(this))
  registerEditingBehavior("es-graph-delete", new GraphDeleteNodeOrLink(this))
  registerEditingBehavior("es-graph-connect-stepto", new GraphConnectStepTo(this))
  registerEditingBehavior("es-graph-examine", new GraphExamineNodes(this))
  
  behaviors.foreach{ case (n: String, b: GraphEditBehavior) =>
    d3.select("#"+n).on("click", {_: EventTarget => setEditingBehavior(n)})
  }
  
  setEditingBehavior("es-graph-examine")
  
  val force = d3.layout.force[GraphNode, NodeLink] ()
      .charge(-300.0)
      .chargeDistance(200.0)
      .linkStrength(0.3)
      .size((400.0, 400.0))
      .gravity(.2)
      .nodes(nodes)
      .links(links)
      
  val graphBendingButton = d3.selectAll("#es-graph-arrowbending")
  graphBendingButton
    .on("change", (a: EventTarget, b: Int) => {
      GraphView.graphBending = graphBendingButton.node.asInstanceOf[HTMLInputElement].checked
      force.resume()
      println("set bending: " + GraphView.graphBending)
    }
  )
  
  val layerLinks = sceneRoot.append("g").classed("layer-links", true)
  val layerNodes = sceneRoot.append("g").classed("layer-nodes", true)
  val layerMeta = sceneRoot.append("g").classed("layer-meta", true)
      
  //var linkViews: Selection[NodeLink] = layerLinks.selectAll(".link")
  var linkPartViews: Selection[LinkViewPart] = layerLinks.selectAll(".link")
  var linkLabelViews: Selection[NodeLink] = layerMeta.selectAll(".link-label")
  
  var nodeViews: Selection[GraphNode] = layerNodes.selectAll(".node")
  var nodeLabelViews: Selection[GraphNode] = layerMeta.selectAll(".node-label")
  
  var relationViews: Selection[NodeLink] = layerMeta.selectAll(".relation")

  var structure: Option[Structure.TSStructure] = None
  var relation: Iterable[(Iterable[NodeID], Iterable[NodeID])] = List()
  
  def buildGraph(ts: Structure.TSStructure, relation: Iterable[(Iterable[NodeID], Iterable[NodeID])]) = {
    
    val nodes = ts.nodes.map { e =>
      (e, new GraphNode(e, ts.nodeLabeling.getOrElse(e, Structure.emptyLabel)))
    }.toMap
    
    val nodeLinks = for {
      ((e1, e2), ees) <- ts.step.tupleSet.groupBy(t => (t._1, t._3))
      en1 = nodes(e1)
      en2 = nodes(e2)
    } yield new NodeLink('stepto, ees.map(_._2.toActString).mkString(", "), Set(en1), Set(en2))

    val relationLinks = for {
      (e1, e2) <- relation
      en1 = e1 map nodes
      en2 = e2 map nodes
      if (en1.nonEmpty && en2.nonEmpty) // for now exclude this kind of end game nodes... //TODO: Fixme!
    } yield new NodeLink('relation, "", en1.toSet, en2.toSet)
    
    (nodes, nodeLinks ++ relationLinks)
  }
  
  def setStructure() = for { ts <- structure } {
    
    force.stop()
    
    val (sysNodes, nodeLinks) = buildGraph(ts, relation)
    
    val newNodes = sysNodes.values.filter(n => !nodes.exists(n.sameNode(_)))
    val deletedNodes = nodes.filter(n => !sysNodes.isDefinedAt(n.nameId))
    deletedNodes.foreach(n => nodes.remove(nodes.indexOf(n)))
    newNodes.foreach(nodes.push(_))
    nodes.foreach { n => n.updateMeta(ts.nodeLabeling.getOrElse(n.nameId, Structure.emptyLabel), force = true)}
    
    val newLinks = nodeLinks.flatMap(_.integrate(nodes)).filter(l => !links.exists(l.sameLink(_)))
    val deletedLinks = links.filter(l => !nodeLinks.exists(l.sameLink(_)))
    deletedLinks.foreach(l => links.remove(links.indexOf(l)))
    newLinks.foreach(links.push(_))
    
    val linkUp = linkPartViews.data(links.flatMap(_.viewParts), (_: LinkViewPart).toString)
    linkUp.enter()
        .append("path")
        .attr("class", (d: LinkViewPart, i: Int) => "link " + d.link.kind.name)
        .attr("marker-end", (d: LinkViewPart, i: Int) => if (d.isEnd) "url(#" + d.link.kind.name.split(" ")(0) + ")" else "none")
        //.call(dragLink)
        //.on("mousemove", onHover _)
        //.on("mouseout", onHoverEnd _)
    linkUp.exit().remove()
    linkPartViews = layerLinks.selectAll(".link")
    
    val linkLabelUp = linkLabelViews.data(links, (_: NodeLink).toString)
    linkLabelUp.enter()
        .append("text")
        .attr("class", (d: NodeLink, i: Int) => "link-label " + d.kind.name)
        .text((_: NodeLink).label)
    linkLabelUp.exit().remove()
    linkLabelViews = layerMeta.selectAll(".link-label")
    
    val nodeUp = nodeViews.data(nodes, (_:GraphNode).nameId.name)
    nodeUp.enter()
        .append("circle")
        .attr("cx", ((d: GraphNode, i: Int) => d.x))
        .attr("cy", ((d: GraphNode, i: Int) => d.y))
        .attr("r", 7)
        .on("mousemove", onHover _)
        .on("mouseout", onHoverEnd _)
        .on("click", onClick _)
        .call(drag)
    nodeUp.attr("class", ((d: GraphNode, i: Int) => "node " + d.meta.act.map(_.name).mkString(" ")))
    nodeUp.exit().remove()
    nodeViews = layerNodes.selectAll(".node")
        
    val nodeLabelUp = nodeLabelViews
        .data(nodes, (_:GraphNode).nameId.name)
    nodeLabelUp.enter()
        .append("text")
        .attr("class", "node-label")
        .text((_: GraphNode).nameId.name)
    nodeLabelUp.exit().remove()
    nodeLabelViews = layerMeta.selectAll(".node-label")
    
    force.linkDistance((l: NodeLink, d: Double) => l.kind match {
      case _ => 150.0
    })
    
    force.on("tick", updateViews _)
    
    force.start()
    
  }
  /*
  def setRelation(rel: LabeledRelation[NodeID, String]) {
    force.stop()
    val relationLinks = for {
      (e1, l, e2) <- rel.tupleSet
      en1 <- nodes.find(_.nameId == e1)
      en2 <- nodes.find(_.nameId == e2)
    } yield new NodeLink(Symbol("relation " + l), "", Set(en1), Set(en2))
    
    val newRealtionLinks = relationLinks.filter(l => !relation.exists(l.sameLink(_)))
    val deletedRelationLinks = relation.filter(l => !relationLinks.exists(l.sameLink(_)))
    deletedRelationLinks.foreach(l => relation.remove(relation.indexOf(l)))
    newRealtionLinks.foreach(relation.push(_))
    
    val relationUp = relationViews.data(relation, (_: NodeLink).toString)
    relationUp.enter()
        .append("path")
        .attr("class", (d: NodeLink, i: Int) => "relation " + d.kind.name)
        .attr("marker-end", (d: NodeLink, i: Int) => "url(#" + d.kind.name.split(" ")(0) + ")")

    relationUp.exit().remove()
    relationViews = layerMeta.selectAll(".relation")
    
    force.start()
  }*/
  
  def setComment(comment: String) = {
    d3.select("#es-graph-comment")
      .text(comment)
      .classed("hidden", comment.isEmpty())
  }
  
  def colorize(partition: Coloring[NodeID]) {
    val colorScale = d3.scale.category20()
    nodeViews.style("stroke", { (d: GraphNode, i: Int) =>
      val repHash = partition(d.nameId)
      colorScale(repHash.toString)
    })
  }
  
  def updateViews(event: dom.Event) {
    nodes.foreach(_.updatePos())
    links foreach { d: NodeLink =>
      d.updateDirAndCenter()
    }
    
    nodeViews
      .attr("cx", ((d: GraphNode, i: Int) => d.x))
      .attr("cy", ((d: GraphNode, i: Int) => d.y))
    nodeLabelViews
      .attr("x", ((d: GraphNode, i: Int) => d.x.get + 4))
      .attr("y", ((d: GraphNode, i: Int) => d.y.get - 10))
    linkPartViews
      .attr("d", (_: LinkViewPart).toSVGPathString)
    linkLabelViews
      .attr("x", (_: NodeLink).center._1)
      .attr("y", (_: NodeLink).center._2)
    relationViews
      .attr("d", (_: NodeLink).toSVGPathString)
  }
  
  override def onSelectionChange() {
    nodeViews.classed("selected", { n: GraphNode =>
      n.selected
    })
  }
  
  def notify(change: ModelComponent.Change) = change match {
    case Structure.StructureChange(structure) =>
      setComment("")
      this.structure = Some(structure)
      relation = List()
      setStructure()
    case Structure.StructurePartitionChange(partition) =>
      colorize(partition)
    case Structure.StructureRelationChange(relation) =>
      //setRelation(relation)
    case Structure.StructureRichRelationChange(relation) =>
      this.relation = relation 
      setStructure()
    case Structure.StructureCommentChange(comment) =>
      setComment(comment)
    case _ =>
      
  }
  
}