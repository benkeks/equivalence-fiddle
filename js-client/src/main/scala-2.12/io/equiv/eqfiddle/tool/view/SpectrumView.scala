package io.equiv.eqfiddle.tool.view

import scala.scalajs.js
import org.scalajs.dom
import org.singlespaced.d3js.d3
import js.JSConverters._
import org.singlespaced.d3js.Ops.fromFunction2To3DoublePrimitive
import org.singlespaced.d3js.Ops.fromFunction2To3StringPrimitive

import io.equiv.eqfiddle.hml.ObservationNotion
import io.equiv.eqfiddle.hml.Spectrum
import io.equiv.eqfiddle.util.Relation

class SpectrumView[+OC <: ObservationNotion](
    spectrum: Spectrum[OC],
    preords: List[String],
    postords: List[String],
    equations: List[String],
    distCoordsLR: List[(OC, String)],
    distCoordsRL: List[(OC, String)],
    parentId: String) {
  val width = 665
  val height = 405

  val axes = if (spectrum.getDimensionality() > 6)
    List(
      (0,-12), // obs
      (-30,-12), // branch
      (-35,-12), // unstable conj
      (115,-22), // stable conj
      (-15,-8),
      (-30,-10),
      (-35,-10),
      (25,-30),
      (15,-25),
    )
  else
    List(
      (0,-15), // obs
      (-30,-20), // conj
      (-10,-20), // max pos conjuncts
      (-20,-30), // other pos conjuncts
      (40,-35), // neg conjuncts
      (20,-25) // negations
    )

  val svg = d3.select(parentId)
    .append("svg")
      .attr("width", width)
      .attr("height", height)
    .append("g")
      .attr("transform",
          s"translate(${width / 2 + 10}, ${height - 20})")

  render()

  def positionOfNotion(oc: ObservationNotion): (Double, Double) = {
    var x = 0.0
    var y = 0.0
    val components = oc.toTuple

    for {
      i <- 0 until components.productArity
      component = components.productElement(i)
      if component.isInstanceOf[Int]
    } {
      val axis = axes(i)
      val componentInt = component.asInstanceOf[Int]
      val componentStrength = if (componentInt > 2) 2.5 else componentInt
      x = x + Math.min(componentStrength * axis._1, 210)
      y = y + componentStrength * axis._2
    }

    (x, y)
  }

  def southOfEquivalenceBoundary(oc: ObservationNotion): Boolean = {
    equations.exists(e => oc <= spectrum.getSpectrumClass(e).obsNotion)
  }

  def southOfPreorderBoundary(oc: ObservationNotion): Boolean = {
    preords.exists(e => oc <= spectrum.getSpectrumClass(e).obsNotion)
  }

  def southOfPostorderBoundary(oc: ObservationNotion): Boolean = {
    postords.exists(e => oc <= spectrum.getSpectrumClass(e).obsNotion)
  }

  def render() = {

    // draw lattice structure

    val positions =
      spectrum.notions.map(_.obsNotion)

    val neighbors = for {
      p1 <- positions
      p2 <- positions
      if (p1 < p2) && positions.forall(otherP => !(p1 < otherP && otherP < p2))
    } yield (p1, p2)

    val links = svg.append("g")
      .selectAll(".eq-links")
      .data(neighbors.toJSArray)
      .enter()
      .append("path")
        .attr("d", (oc1oc2: (OC, OC), _: Int) => {
          val (oc1, oc2) = oc1oc2
          val (x1, y1) = positionOfNotion(oc1)
          val (x2, y2) = positionOfNotion(oc2)
          s"M${x1} ${y1} L ${x2} ${y2}"
        })
        .style("stroke", "black")
        .style("stroke-opacity", 0.3)
        .style("stroke-width", 2.0)

    // mark which distinctions refute which closest notions

    val distinctionNeighbors = for {
      (p1, _) <- distCoordsLR ++ distCoordsRL
      p2 <- positions
      if (p1 <= p2) && positions.forall(otherP => !(p1 < otherP) || !(otherP < p2))
    } yield (p1, p2)

    val distinctionsAffect = new Relation(distinctionNeighbors.toSet)
    val distinctionRenderPos = {
      for {
        p1 <- distinctionsAffect.lhs
        affected = distinctionsAffect.values(p1)
        renderPositions = affected.map(positionOfNotion(_))
        renderX = renderPositions.map(_._1).sum / renderPositions.size
        renderY = (renderPositions.map(_._2).sum / renderPositions.size) + renderPositions.size * 15
      } yield (p1, (renderX, renderY))
    }.toMap

    val distinctionLinks = svg.append("g")
      .selectAll(".eq-distinction-links")
      .data(distinctionNeighbors.toJSArray)
      .enter()
      .append("path")
        .attr("d", (oc1oc2: (OC, OC), _: Int) => {
          val (oc1, oc2) = oc1oc2
          val (x1, y1) = distinctionRenderPos(oc1)
          val (x2, y2) = positionOfNotion(oc2)
          s"M${x1} ${y1} L ${x2} ${y2}"
        })
        .style("stroke", "#cc1100")

    // show notions

    val dots = svg.append("g")
      .selectAll(".eq-notion-dot")
      .data(spectrum.notions.toJSArray)
      .enter()
      .append("circle")
        .attr("cx", (eq: Spectrum.EquivalenceNotion[OC], _: Int) => positionOfNotion(eq.obsNotion)._1 )
        .attr("cy", (eq: Spectrum.EquivalenceNotion[OC], _: Int) => positionOfNotion(eq.obsNotion)._2 )
        .attr("r", 4)
        .style("fill", (eq: Spectrum.EquivalenceNotion[OC], _: Int) =>
          if (southOfEquivalenceBoundary(eq.obsNotion)) "#1177dd" else "#992211")
        .style("stroke-width", (eq: Spectrum.EquivalenceNotion[OC], _: Int) =>
          if (equations.contains(eq.name)) 4.0 else 0.0)
        .style("stroke", "#33aaff")
        .html((eq: Spectrum.EquivalenceNotion[OC], _: Int, _: js.UndefOr[Int]) =>
          s"<title>${eq.obsNotion.toTuple}</title>".replaceAll(Int.MaxValue.toString(),"∞"))

    val strictPreords = spectrum.notions.filter(n => southOfPreorderBoundary(n.obsNotion) && !southOfEquivalenceBoundary(n.obsNotion))
    val strictPostords = spectrum.notions.filter(n => southOfPostorderBoundary(n.obsNotion) && !southOfEquivalenceBoundary(n.obsNotion))

    val preorderSymbs = svg.append("g")
      .selectAll(".eq-notion-preorder")
      .data(strictPreords.toJSArray)
      .enter()
      .append("path")
        .attr("d", (eq: Spectrum.EquivalenceNotion[OC], _: Int) => {
          val (x, y) = positionOfNotion(eq.obsNotion)
          s"M${x} ${y-4} A 1 1 0 0 0 ${x} ${y+4} Z"
        })
        .style("fill", "#1177dd")
        .style("stroke", "#33aaff")
        .style("pointer-events", "none")
    
    val postorderSymbs = svg.append("g")
      .selectAll(".eq-notion-postorder")
      .data(strictPostords.toJSArray)
      .enter()
      .append("path")
        .attr("d", (eq: Spectrum.EquivalenceNotion[OC], _: Int) => {
          val (x, y) = positionOfNotion(eq.obsNotion)
          s"M${x} ${y-4} A 1 1 0 0 1 ${x} ${y+4} Z"
        })
        .style("fill", "#1177dd")
        .style("stroke", "#33aaff")
        .style("pointer-events", "none")

    val lrDistinctions = svg.append("g")
      .selectAll(".eq-dist-lr")
      .data(distCoordsLR.toJSArray)
      .enter()
      .append("polygon")
        .attr("points", (oc: (OC, String), _: Int) => {
          val (x, y) = distinctionRenderPos(oc._1)
          s"${x-3},${y-4} ${x+5},${y} ${x-3},${y+4}"
         })
        .style("fill", "#cc1100")
        .html((oc: (OC, String), _: Int, _: js.UndefOr[Int]) => s"<title>${oc._2}</title>")

    val rlDistinctions = svg.append("g")
      .selectAll(".eq-dist-rl")
      .data(distCoordsRL.toJSArray)
      .enter()
      .append("polygon")
        .attr("points", (oc: (OC, String), _: Int) => {
          val (x, y) = distinctionRenderPos(oc._1)
          s"${x+3},${y-4} ${x+3},${y+4} ${x-5},${y}"
         })
        .attr("title", (oc: (OC, String), _: Int) => oc._2)
        .style("fill", "#cc1100")
        .html((oc: (OC, String), _: Int, _: js.UndefOr[Int]) => s"<title>${oc._2}</title>")

    val names = svg.append("g")
      .selectAll(".eq-notion-name")
      .data(spectrum.notions.toJSArray)
      .enter()
      .append("text")
        .attr("x", (eq: Spectrum.EquivalenceNotion[OC], _: Int) => {
          val pos = positionOfNotion(eq.obsNotion)._1
          if (pos < 0) pos - 5 else pos + 5
        })
        .attr("y", (eq: Spectrum.EquivalenceNotion[OC], _: Int) => positionOfNotion(eq.obsNotion)._2 + 5 )
        .attr("text-anchor", (eq: Spectrum.EquivalenceNotion[OC], _: Int) =>
          if (positionOfNotion(eq.obsNotion)._1 < 0) "end" else "start")
        .text((eq: Spectrum.EquivalenceNotion[OC], _: Int) => eq.name)
        .style("pointer-events", "none")

  }
}
