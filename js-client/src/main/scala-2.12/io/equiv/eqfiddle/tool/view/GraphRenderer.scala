package io.equiv.eqfiddle.tool.view

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
import io.equiv.eqfiddle.tool.arch.Control
import io.equiv.eqfiddle.tool.control.ModelComponent
import io.equiv.eqfiddle.tool.control.Structure
import io.equiv.eqfiddle.util.Partition
import io.equiv.eqfiddle.tool.model.NodeID
import io.equiv.eqfiddle.util.Coloring
import io.equiv.eqfiddle.util.Relation
import io.equiv.eqfiddle.util.LabeledRelation
import io.equiv.eqfiddle.hml.Spectrum
import io.equiv.eqfiddle.hml.ObservationClass

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
  
  setEditingBehavior("es-graph-move")
  
  val force = d3.layout.force[GraphNode, NodeLink] ()
      .charge(-100.0)
      .chargeDistance(150.0)
      .linkStrength(0.3)
      .size((700.0, 700.0))
      .gravity(.02)
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
  
  val layerNodes = sceneRoot.append("g").classed("layer-nodes", true)
  val layerLinks = sceneRoot.append("g").classed("layer-links", true)
  val layerMeta = sceneRoot.append("g").classed("layer-meta", true)
      
  //var linkViews: Selection[NodeLink] = layerLinks.selectAll(".link")
  var linkPartViews: Selection[LinkViewPart] = layerLinks.selectAll(".link")
  var linkLabelViews: Selection[NodeLink] = layerMeta.selectAll(".link-label")
  
  var nodeViews: Selection[GraphNode] = layerNodes.selectAll(".node")
  var nodeLabelViews: Selection[GraphNode] = layerMeta.selectAll(".node-label")
  
  var relationViews: Selection[NodeLink] = layerMeta.selectAll(".relation")

  var structure: Option[Structure.TSStructure] = None
  var relation: LabeledRelation[(Set[NodeID], String, Set[NodeID]), String] = LabeledRelation()
  var baseRelation: Set[(Set[NodeID], String, Set[NodeID])] = Set()
  
  def buildGraph(ts: Structure.TSStructure) = {
    
    val nodes = ts.nodes.map { e =>
      (e, new GraphNode(e, ts.nodeLabeling.getOrElse(e, Structure.emptyLabel)))
    }.toMap + (GraphView.dummyNode.nameId -> GraphView.dummyNode)
    
    val nodeLinks = for {
      ((e1, e2), ees) <- ts.step.tupleSet.groupBy(t => (t._1, t._3))
      en1 = nodes(e1)
      en2 = nodes(e2)
    } yield new NodeLink('stepto, ees.map(_._2.toActString).mkString(", "), Set(en1), Set(en2), (en1, en2))

    val relationLinks = for {
      ((e1, e2), ll) <- baseRelation.groupBy(t => (t._1, t._3)).toIterable
      en1 = e1 map nodes
      en2 = e2 map nodes
      (l0, i) <- ll.toList.zipWithIndex
      l = (" " * i) + l0._2
    } yield new NodeLink(Symbol("relation " + l0._2), l, en1.toSet, en2.toSet, (e1, l0._2, e2))

    val relationMetaLinks = for {
      ((e1, e2), ll) <- relation.tupleSet.groupBy(t => (t._1, t._3))
      en1 = relationLinks filter (_.hasRep(e1))
      en2 = relationLinks filter (_.hasRep(e2))
      l = ll.map(_._2).mkString(", ")
    } yield new NodeLink(Symbol("relation ho"), l, en1.toSet, en2.toSet, (e1, l, e2))

    (nodes, nodeLinks ++ relationLinks, relationMetaLinks)
  }
  
  def setStructure() = for { ts <- structure } {
    
    force.stop()
    
    val (sysNodes, nodeLinks, hoLinks) = buildGraph(ts)
    
    val newNodes = sysNodes.values.filter(n => !nodes.exists(n.sameRep(_)))
    val deletedNodes = nodes.filter(n => !sysNodes.isDefinedAt(n.nameId))
    deletedNodes.foreach(n => nodes.remove(nodes.indexOf(n)))
    newNodes.foreach(nodes.push(_))
    nodes.foreach { n => n.updateMeta(ts.nodeLabeling.getOrElse(n.nameId, Structure.emptyLabel), force = true)}
    
    val nodesAndLinks = nodes ++ links
    val newLinks = nodeLinks.flatMap(_.integrate(nodesAndLinks)).filter(l => !links.exists(l.sameRep(_)))
    val nodesAndLinks2 = nodesAndLinks ++ newLinks
    val newHoLinks = hoLinks.flatMap(_.integrate(nodesAndLinks2)).filter(l => !links.exists(l.sameRep(_)))
    val nodeAndHOLinks = nodeLinks ++ hoLinks
    val deletedLinks = links.filter(l => !nodeAndHOLinks.exists(l.sameRep(_)))
    deletedLinks.foreach(l => links.remove(links.indexOf(l)))
    newLinks.foreach(links.push(_))
    newHoLinks.foreach(links.push(_))
    
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
    
    val nodeUp = nodeViews.data(nodes - GraphView.dummyNode, (_:GraphNode).nameId.name)
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
        .data(nodes - GraphView.dummyNode, (_:GraphNode).nameId.name)
    nodeLabelUp.enter()
        .append("text")
        .attr("class", "node-label")
        .text((_: GraphNode).nameId.name)
    nodeLabelUp.exit().remove()
    nodeLabelViews = layerMeta.selectAll(".node-label")

    nodes.foreach(_.positionStealTarget = None)
    for {
      (ee1, l, ee2) <- baseRelation
      if l == "eq"
      e1 <- ee1
      e2 <- ee2
      n1 <- nodes.find(_.nameId == e1)
      n2 <- nodes.find(_.nameId == e2)
    } {
      n2.positionStealTarget = Some(n1)
    }

    force.linkDistance((l: NodeLink, d: Double) => l.kind match {
      case _ => 80.0
    })
    force.alpha(1)

    force.on("tick", updateViews _)
    
    force.start()
    
  }

  def setComment(comment: String) = {
    d3.select("#es-graph-comment")
      .html(comment)
      .classed("hidden", comment.isEmpty())
  }

  def showSpectrum(spectrum: Spectrum[ObservationClass], componentId: String) = {
    val spectrumView = new SpectrumView(spectrum, componentId)
  }

  def colorize(partition: Coloring[NodeID]) {
    val colorScale = d3.scale.category20()
    nodeViews.style("stroke", { (d: GraphNode, i: Int) =>
      for (repHash <- partition.get(d.nameId)) {
        colorScale(repHash.toString)
      }
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
      .attr("x", (_: NodeLink).centerX)
      .attr("y", (_: NodeLink).centerY)
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
      relation = LabeledRelation()
      baseRelation = Set()
      setStructure()
    case Structure.StructurePartitionChange(partition) =>
      colorize(partition)
    case Structure.StructureRelationChange(relation) =>
      this.relation = LabeledRelation()
      this.baseRelation = relation.tupleSet.map(t => (Set(t._1), t._2, Set(t._3)))
      setStructure()
    case Structure.StructureRichRelationChange(relation) =>
      this.relation = relation
      this.baseRelation = relation.lhs ++ relation.rhs
      setStructure()
    case Structure.StructureCommentChange(comment) =>
      setComment(comment)
    case Structure.StructureSpectrumChange(spectrum, comment) =>
      setComment(comment + """<div id="es-spectrum-view"></div>""")
      showSpectrum(spectrum, "#es-spectrum-view")
    case _ =>
      
  }
  
}