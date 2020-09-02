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
      var source: GraphNode, 
      var target: GraphNode)
    extends Link[GraphNode] with LinkTrait {
     
    var length: Double = 0
    
    var dir: (Double, Double) = (0,0)
    
    var center: (Double, Double) = (0,0)
    
    def updateDirAndCenter() {
      if (source.nameId == target.nameId) {
        // loop edge!
        length = 30
        dir = (-1,0)
        center = (target.x.get + 20 * 1.5, target.y.get + 20 * 1.5)
      } else {
        length = Math.hypot(target.x.get - source.x.get, target.y.get - source.y.get)
        dir = ((target.x.get - source.x.get) / length, (target.y.get - source.y.get) / length)
        center = ((target.x.get + source.x.get) / 2, (target.y.get + source.y.get) / 2)
      }
    }
    
    /**
     * integrates the nodes into an existing graph
     */
    def integrate(nodes: Iterable[GraphNode]): Option[NodeLink] = {
      for {
        newSrc <- nodes.find(source.sameNode(_))
        newTar <- nodes.find(target.sameNode(_))
      } yield new NodeLink(kind, label, newSrc, newTar)
    }
    
    def toSVGPathString = (
      if (source == target) { // loop
       "M"   + (source.x.get + 9) + " " + (source.y.get) +
       "A 20 20, 0, 1, 1, " + (target.x.get) + " " + (target.y.get + 9)
      } else { // line
       "M"   + (source.x.get + 9 * dir._1) + " " + (source.y.get + 9 * dir._2) +
       "L"   + (target.x.get - 9 * dir._1) + " " + (target.y.get - 9 * dir._2)
      }
    )
    
    override def toString = source.toString + "-" + kind + "-" + label + "-" + target.toString
    
    override def hashCode = 23 * kind.hashCode + 39 * source.hashCode + target.hashCode
    
    def sameLink(l: LinkTrait) = l match {
      case o: NodeLink =>
        (o.kind equals kind) &&
          o.label == label &&
          o.source.nameId == source.nameId &&
          o.target.nameId == target.nameId
      case _ =>
        false
    }
  }
  
}

trait GraphView {
  val nodes = js.Array[GraphView.GraphNode]()
  val links = js.Array[GraphView.NodeLink]()
  
  val relation = js.Array[GraphView.NodeLink]()
}
