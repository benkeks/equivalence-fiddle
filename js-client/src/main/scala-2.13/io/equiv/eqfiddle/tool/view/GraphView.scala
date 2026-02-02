package io.equiv.eqfiddle.tool.view

import scala.scalajs.js
import scala.scalajs.js.UndefOr
import d3v4._
import io.equiv.eqfiddle.tool.control.Structure
import io.equiv.eqfiddle.tool.model.NodeID

object GraphView {
  var graphBending = false
  
  val dummyNode = new GraphNode(NodeID("#dummy#"), Structure.NodeLabel(Set(), Option(0), Option(0)))

  trait Linkable {
    def centerX: Double
    def centerY: Double

    def sameRep(a: Linkable): Boolean

    def hasRep(a: Any): Boolean
  }

  class GraphNode(
      var nameId: NodeID,
      var meta: Structure.NodeLabel,
      var positionStealTarget: Option[Linkable] = None)
    extends SimulationNodeImpl with Linkable {
    
    GraphNode.count = GraphNode.count + 1
    
    var fixedPermanently = meta.x.isDefined && meta.y.isDefined
    
    var id: Double = GraphNode.count
    
    var selected = false
    var previouslySelected = false
    var hovered = false
    
    x = 100 + Math.cos(GraphNode.count * 5.1) * 100
    y = 100 + Math.sin(GraphNode.count * 5.1) * 100
    
    updateMeta(meta, true)
    
    def updateMeta(metaInfo: Structure.NodeLabel, force: Boolean = false) = {
      if (meta != metaInfo || force) {
        meta = metaInfo
        val newX: Double = meta.x.getOrElse(x.getOrElse(0.0))
        val newY: Double = meta.y.getOrElse(y.getOrElse(0.0))
        x = newX
        y = newY
        fixedPermanently = meta.x.isDefined && meta.y.isDefined
        fx = if (fixedPermanently) newX else js.undefined
        fy = if (fixedPermanently) newY else js.undefined
      }
    }
    
    def updatePos() = {
      if (positionStealTarget.nonEmpty) {
        for {
          tarX <- positionStealTarget.map(_.centerX) orElse meta.x
          currX <- x.toOption
        } {
          val xDiff = tarX - currX
          if (Math.abs(xDiff) < 15.0) {
            x = tarX
            fx = tarX
          } else {
            x = currX + 10.0 * Math.signum(xDiff)
            fx = js.undefined
          }
        }
        for {
          tarY <- positionStealTarget.map(_.centerY) orElse meta.y
          currY <- y.toOption
        } {
          val yDiff = tarY - currY
          if (Math.abs(yDiff) < 15.0) {
            y = tarY
            fy = tarY
          } else {
            y = currY + 10.0 * Math.signum(yDiff)
            fy = js.undefined
          }
        }
      }
    }
    
    override def sameRep(a: Linkable) = a match {
      case o: GraphNode =>
        o.nameId equals nameId
      case _ =>
        false
    }

    override def hasRep(a: Any) = (nameId == a)
    
    override def centerX = x.getOrElse(0)
    override def centerY = y.getOrElse(0)

    override def hashCode = nameId.hashCode
    
    override def toString = s"$id${nameId.name}"
  }
  
  object GraphNode {
    var count: Double = 0.0
  }
    
  class NodeLink(
      var kind: Symbol, 
      var label: String,
      var sources: Set[Linkable],
      var targets: Set[Linkable],
      var rep: Any)
    extends SimulationLinkImpl[GraphNode, GraphNode] with Linkable {
    
    var source = sources.collect { case gn: GraphNode => gn }.headOption.getOrElse(dummyNode)

    var target = targets.collect { case gn: GraphNode => gn }.headOption.getOrElse(dummyNode)

    var length: Double = 0.1
    
    var dir: (Double, Double) = (0.0, 0.0)

    val isLoop = sources == targets

    val hasDanglingEnd = targets.isEmpty

    val bend = (if (hasDanglingEnd) 0.0 else .5) + label.takeWhile(_.isWhitespace).length
    
    var srcCenter: (Double, Double) = if (sources.nonEmpty) (
      (sources.map(_.centerX).sum / sources.size),
      (sources.map(_.centerY).sum / sources.size)
    ) else (0.0, 0.0)

    var tarCenter: (Double, Double) = if (targets.nonEmpty) (
      (targets.map(_.centerX).sum / targets.size),
      (targets.map(_.centerY).sum / targets.size)
    ) else (srcCenter._1 + 50.0, srcCenter._2 + 50.0)
    
    override def centerX = (tarCenter._1 * (.5 - .02 * bend) + srcCenter._1 * (.5 + .02 * bend)) - 10 * (bend + 0.00001 * length * length) * dir._2
    override def centerY = (tarCenter._2 * (.5 - .02 * bend) + srcCenter._2 * (.5 + .02 * bend)) + 10 * (bend + 0.00001 * length * length) * dir._1

    val viewParts = {
      sources.map(s => new LinkViewPart(this, s, isEnd = targets.contains(s))) ++
      targets.map(new LinkViewPart(this, _)) ++
      (if (targets.isEmpty) List(new LinkViewPart(this, dummyNode)) else List())
    }

    def updateDirAndCenter(): Unit = {
      srcCenter = if (sources.nonEmpty) (
        (sources.map(_.centerX).sum / sources.size),
        (sources.map(_.centerY).sum / sources.size)
      ) else (
        tarCenter._1 - 50,
        tarCenter._2 - 50
      )
      tarCenter = if (targets.nonEmpty) (
        (targets.map(_.centerX).sum / targets.size),
        (targets.map(_.centerY).sum / targets.size)
      ) else (
        srcCenter._1 + 50,
        srcCenter._2 + 50
      )
      length = Math.hypot(tarCenter._1 - srcCenter._1, tarCenter._2 - srcCenter._2)
      dir = if (isLoop || length <= 0.0001) (
        (1.0, 0.0)
      ) else (
        (tarCenter._1 - srcCenter._1) / length,
        (tarCenter._2 - srcCenter._2) / length
      )
    }

    def integrate(nodes: Iterable[Linkable]): Option[NodeLink] = {
      val newSrc = sources.flatMap { n => nodes.find(n.sameRep(_)) }
      val newTar = targets.flatMap { n => nodes.find(n.sameRep(_)) }
      Some(new NodeLink(kind, label, newSrc, newTar, rep))
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
    
    def sameRep(l: Linkable) = l.hasRep(rep)

    override def hasRep(a: Any) = rep == a
  }

  class LinkViewPart(val link: NodeLink, val node: Linkable, val isEnd: Boolean = true) {

    private val endShortening = if (node.isInstanceOf[GraphNode]) 10.0 else 0

    def toSVGPathString = {
      if (link.tarCenter._1.isNaN()) throw new Exception("NaN tar!")
      if (link.dir._1.isNaN()) throw new Exception("NaN dir!")
      if (node.centerX.isNaN()) throw new Exception("NaN center!")
      if (isEnd) {
        if (link.sources.contains(node)) {
          "M"   + (node.centerX + 9) + " " + (node.centerY) +
            "A 30 30, 0, 1, 1, " + (link.centerX) + " " + (link.centerY + 9)
        } else if (link.hasDanglingEnd) {
          "M "   + link.centerX +","+ link.centerY + 
              " Q " + (link.centerX + .2 * link.length * link.dir._1) +","+ (link.centerY + .2 * link.length * link.dir._2)+
              " " + (.5 * (link.centerX + link.tarCenter._1)) +","+ (.5 * (link.centerY + link.tarCenter._2))
        } else {
          "M "   + link.centerX       +","+ link.centerY + 
              " C " + (link.centerX + .2 * link.length * link.dir._1) +","+ (link.centerY + .2 * link.length * link.dir._2)+
              " " + (.5 * (link.centerX + link.tarCenter._1)) +","+ (.5 * (link.centerY + link.tarCenter._2)) +
              " " + (node.centerX - endShortening * link.dir._1) +","+ (node.centerY - endShortening * link.dir._2)
        }
      } else {
        if (link.targets.contains(node)) {
          "M"   + (link.centerX + 9) + " " + (link.centerY) +
            "A 30 30, 0, 1, 1, " + (node.centerX) + " " + (node.centerY + 9)
        } else {
          "M " + link.centerX       +","+ link.centerY +
              " C" + (link.centerX - .3 * link.length * link.dir._1) +","+ (link.centerY - .3 * link.length * link.dir._2)+
              " " + (.5 * (link.centerX + link.srcCenter._1)) +","+ (.5 * (link.centerY + link.srcCenter._2)) +
              " " + (node.centerX + endShortening * link.dir._1)  +","+ (node.centerY + endShortening * link.dir._2)
        }
      }
    }
    
    override def toString = s"$node::$link"
  }

  
}

trait GraphView {
  val nodes = js.Array[GraphView.GraphNode]()
  val links = js.Array[GraphView.NodeLink]()
}
