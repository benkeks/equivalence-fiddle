package de.bbisping.coupledsim.tool.view

import scala.scalajs.js
import scala.scalajs.js.UndefOr
import scala.scalajs.js.UndefOr.any2undefOrA
import scala.scalajs.js.UndefOr.undefOr2ops
import org.singlespaced.d3js.Link
import org.singlespaced.d3js.forceModule.Node
import de.bbisping.coupledsim.tool.control.Structure
import de.bbisping.coupledsim.tool.model.NodeID

object GraphView {
  var graphBending = false
  
  class GraphNode(
      var nameId: NodeID,
      var meta: Structure.NodeLabel)
    extends Node {
    
    GraphNode.count = GraphNode.count + 1
    
    var fixedPermanently = meta.x.isDefined && meta.y.isDefined
    
    var id: Double = GraphNode.count
    
    var selected = false
    var previouslySelected = false
    
    x = 100 + Math.cos(GraphNode.count * 5.1) * 100
    y = 100 + Math.sin(GraphNode.count * 5.1) * 100
    weight = 1.0
    
    updateMeta(meta, true)
    
    def updateMeta(metaInfo: Structure.NodeLabel, force: Boolean = false) = {
      if (meta != metaInfo || force) {
        meta = metaInfo
        x = UndefOr.any2undefOrA(meta.x getOrElse x.get)
        y = UndefOr.any2undefOrA(meta.y getOrElse y.get)
        px = x
        py = y
        fixedPermanently = meta.x.isDefined && meta.y.isDefined
        fixed = if (fixedPermanently) 1 else 0
      }
    }
    
    def updatePos() = {
      if (fixedPermanently && fixed.get <= 1.5) {
        fixed = 1
        for (tarX <- meta.x; currX <- x.toOption) {
          val xDiff = tarX - currX
          if (Math.abs(xDiff) < 2.0) {
            x = UndefOr.any2undefOrA(tarX)
          } else {
            x = UndefOr.any2undefOrA(currX + 2.0 * Math.signum(xDiff))
            fixed = 0
          }
        }
        for (tarY <- meta.y; currY <- y.toOption) {
          val yDiff = tarY - currY
          if (Math.abs(yDiff) < 2.0) {
            y = UndefOr.any2undefOrA(tarY)
          } else {
            y = UndefOr.any2undefOrA(currY + 2.0 * Math.signum(yDiff))
            fixed = 0
          }
        }
      }
    }
    
    def sameNode(o: GraphNode) = o != null && (o.nameId equals nameId) // && (o.meta equals meta)
    
    override def hashCode = nameId.hashCode
    
    override def toString = id + nameId.name
  }
  
  object GraphNode {
    var count: Double = 0.0
  }
  
  trait LinkTrait {
    def sameLink(o: LinkTrait): Boolean
    
    def center: (Double, Double)
  }
  
  class NodeLink(
      var kind: Symbol, 
      var label: String,
      var sources: Set[GraphNode],
      var targets: Set[GraphNode])
    extends Link[GraphNode] with LinkTrait {
    
    val source = sources.head

    val target = targets.head

    var length: Double = 0
    
    var dir: (Double, Double) = (0,0)
    
    var srcCenter: (Double, Double) = (
      (sources.map(_.x.get).sum / sources.size),
      (sources.map(_.y.get).sum / sources.size)
    )

    var tarCenter: (Double, Double) = (
      (targets.map(_.x.get).sum / targets.size),
      (targets.map(_.y.get).sum / targets.size)
    )
    
    var center: (Double, Double) = (
      (tarCenter._1 + srcCenter._1) / 2,
      (tarCenter._2 + srcCenter._2) / 2
    )

    val viewParts = {
      sources.map(new LinkViewPart(this, _, isEnd = false)) ++
      targets.map(new LinkViewPart(this, _))
    }

    def updateDirAndCenter() {
      /*if (source.nameId == target.nameId) {
        // loop edge!
        length = 30
        dir = (-1,0)
        center = (target.x.get + 20 * 1.5, target.y.get + 20 * 1.5)
      } else {
        length = Math.hypot(target.x.get - source.x.get, target.y.get - source.y.get)
        dir = ((target.x.get - source.x.get) / length, (target.y.get - source.y.get) / length)
        center = ((target.x.get + source.x.get) / 2, (target.y.get + source.y.get) / 2)
      }*/
      srcCenter = (
        (sources.map(_.x.get).sum / sources.size),
        (sources.map(_.y.get).sum / sources.size)
      )
      tarCenter = (
        (targets.map(_.x.get).sum / targets.size),
        (targets.map(_.y.get).sum / targets.size)
      )
      length = Math.hypot(tarCenter._1 - srcCenter._1, tarCenter._2 - srcCenter._2)
      dir = (
        (tarCenter._1 - srcCenter._1) / length,
        (tarCenter._2 - srcCenter._2) / length
      )
      center = (
        (tarCenter._1 + srcCenter._1) / 2,
        (tarCenter._2 + srcCenter._2) / 2
      )
    }
    
    /**
     * integrates the nodes into an existing graph
     */
    def integrate(nodes: Iterable[GraphNode]): Option[NodeLink] = {
      val newSrc = sources.flatMap { n => nodes.find(n.sameNode(_)) }
      val newTar = targets.flatMap { n => nodes.find(n.sameNode(_)) }
      if (newSrc.nonEmpty && newTar.nonEmpty) {
        Some(new NodeLink(kind, label, newSrc, newTar))
      } else {
        None
      }
    }
    
    def toSVGPathString = (
      ""
      /*if (source == target) { // loop
       "M"   + (source.x.get + 9) + " " + (source.y.get) +
       "A 20 20, 0, 1, 1, " + (target.x.get) + " " + (target.y.get + 9)
      } else { // line
       "M"   + (source.x.get + 9 * dir._1) + " " + (source.y.get + 9 * dir._2) +
       "L"   + (target.x.get - 9 * dir._1) + " " + (target.y.get - 9 * dir._2)
      }*/
    )
    
    override def toString = sources.toString + "-" + kind + "-" + label + "-" + targets.toString
    
    override def hashCode = 23 * kind.hashCode + 39 * sources.hashCode + targets.hashCode
    
    def sameLink(l: LinkTrait) = l match {
      case o: NodeLink =>
        (o.kind equals kind) &&
          o.label == label &&
          o.sources.map(_.nameId) == sources.map(_.nameId) &&
          o.targets.map(_.nameId) == targets.map(_.nameId)
      case _ =>
        false
    }
  }

  /*class LinkLink(
      val kind: Symbol,
      var source: List[GraphNode],
      var target: LinkTrait)
    extends LinkTrait {
    
    def integrate(nodes: Iterable[GraphNode], links: Iterable[LinkTrait]): Option[LinkLink] = {
      val newSrc = source.flatMap(s => nodes.find(s.sameNode(_)))
      for (
        newTar <- links.find(target.sameLink(_))
      ) yield new LinkLink(kind, newSrc, newTar)
    }
    
    var srcCenter: (Double, Double) = (
      (source.map(_.x.get).sum / source.length),
      (source.map(_.y.get).sum / source.length)
    )
    
    var center: (Double, Double) = (
      (target.center._1 + srcCenter._1) / 2 + 10.0,
      (target.center._2 + srcCenter._2) / 2
    )
    
    updatePos()
    
    def updatePos() = {
      srcCenter = (
        (source.map(_.x.get).sum / source.length),
        (source.map(_.y.get).sum / source.length)
      )
      center = (
        (target.center._1 + srcCenter._1) / 2,
        (target.center._2 + srcCenter._2) / 2
      )
      // bending of higher order arrows
      if (graphBending) {
        val dx = target.center._1 - srcCenter._1
        val dy = target.center._2 - srcCenter._2
        val lengthInv = 25.0 / Math.sqrt(dx * dx + dy * dy)
        center = (
          center._1 - dy * lengthInv,
          center._2 + dx * lengthInv
        )
      }
    }
    
    def sameLink(l: LinkTrait) = l match {
      case o: LinkLink =>
        (o.kind equals kind) &&
        o.source.map(_.esEvent).toSet == source.map(_.esEvent).toSet &&
        o.target.sameLink(target)
      case _ =>
        false
    }
    
    def matchesRule(r: HDEventStructure.Rule): Boolean = {
      savedRule.exists(_ == r)
    }
    
    override def toString = source.toString + "-" + kind + "-" + target.toString
  }*/
  
  class LinkViewPart(val link: NodeLink, val source: GraphNode, val isEnd: Boolean = true) {
    
    def toSVGPathString = if (isEnd) {
      "M"   + link.center._1       +" "+ link.center._2 + 
          " Q" + (link.tarCenter._1 - 15.0 * link.dir._1) +" "+ (link.tarCenter._2 - 15.0 * link.dir._2)+
           " " + (source.x.get - 12.0 * link.dir._1) +" "+ (source.y.get - 12.0 * link.dir._2)
    } else {
      "M" + link.center._1       +" "+ link.center._2 +
           " Q" + link.srcCenter._1    +" "+ link.srcCenter._2 +
           " " + source.x.get             +" "+ source.y.get
    }
    
    override def toString = source + "::" + link
  }

  
}

trait GraphView {
  val nodes = js.Array[GraphView.GraphNode]()
  val links = js.Array[GraphView.NodeLink]()
  
  val relation = js.Array[GraphView.NodeLink]()
}
