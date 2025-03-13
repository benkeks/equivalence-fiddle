package io.equiv.eqfiddle.tool.view

import scala.scalajs.js
import org.scalajs.dom
import org.singlespaced.d3js.d3
import js.JSConverters._
import org.singlespaced.d3js.Ops.fromFunction2To3DoublePrimitive
import org.singlespaced.d3js.Ops.fromFunction2To3StringPrimitive

import io.equiv.eqfiddle.hml.ObservationNotion
import io.equiv.eqfiddle.hml.Spectrum

class SpectrumView[+OC <: ObservationNotion](
    spectrum: Spectrum[OC],
    preords: List[String],
    equations: List[String],
    distCoordsLR: List[(OC, String)],
    distCoordsRL: List[(OC, String)],
    parentId: String) {
  val width = 550
  val height = 405

  val axes = if (spectrum.notions.head.obsClass.toTuple.productArity > 6)
    List(
      (0,-10), // obs
      (-25,-10), // branch
      (-30,-10), // unstable conj
      (50,-25), // stable conj
      (8,-15),
      (-10,-15),
      (-30,-15),
      (15,-25),
      (17,-30),
    )
  else
    List(
      (0,-15), // obs
      (-30,-20), // conj
      (15,-20), // max pos conjuncts
      (-20,-25), // other pos conjuncts
      (40,-25), // neg conjuncts
      (20,-50) // negations
    )

  val svg = d3.select(parentId)
    .append("svg")
      .attr("width", width)
      .attr("height", height)
    .append("g")
      .attr("transform",
          s"translate(${width / 2}, $height)")

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
      x = x + componentStrength * axis._1
      y = y + componentStrength * axis._2
    }

    (x, y)
  }

  def southOfEquivalenceBoundary(oc: ObservationNotion): Boolean = {
    equations.exists(e => oc <= spectrum.getSpectrumClass(e).obsClass)
  }

  def southOfPreorderBoundary(oc: ObservationNotion): Boolean = {
    preords.exists(e => oc <= spectrum.getSpectrumClass(e).obsClass)
  }

  def render() = {

    // draw lattice structure

    val positions =
      spectrum.notions.map(_.obsClass)

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

    // mark which distinctions refute which closest notions

    val distinctionNeighbors = for {
      (p1, _) <- distCoordsLR ++ distCoordsRL
      p2 <- positions
      if (p1 <= p2) && positions.forall(otherP => !(p1 < otherP) || !(otherP < p2))
    } yield (p1, p2)

    val distinctionLinks = svg.append("g")
      .selectAll(".eq-distinction-links")
      .data(distinctionNeighbors.toJSArray)
      .enter()
      .append("path")
        .attr("d", (oc1oc2: (OC, OC), _: Int) => {
          val (oc1, oc2) = oc1oc2
          val (x1, y1) = positionOfNotion(oc1)
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
        .attr("cx", (eq: Spectrum.EquivalenceNotion[OC], _: Int) => positionOfNotion(eq.obsClass)._1 )
        .attr("cy", (eq: Spectrum.EquivalenceNotion[OC], _: Int) => positionOfNotion(eq.obsClass)._2 )
        .attr("r", 4)
        .style("fill", (eq: Spectrum.EquivalenceNotion[OC], _: Int) =>
          if (southOfEquivalenceBoundary(eq.obsClass)) "#1177dd" else if (southOfPreorderBoundary(eq.obsClass)) "#5577aa" else "#992211")
        .style("stroke-width", (eq: Spectrum.EquivalenceNotion[OC], _: Int) =>
          if (equations.contains(eq.name)) 4.0 else 0.0)
        .style("stroke", "#33aaff")
        .html((eq: Spectrum.EquivalenceNotion[OC], _: Int, _: js.UndefOr[Int]) =>
          s"<title>${eq.obsClass.toTuple}</title>".replaceAll(Int.MaxValue.toString(),"∞"))


    val lrDistinctions = svg.append("g")
      .selectAll(".eq-dist-lr")
      .data(distCoordsLR.toJSArray)
      .enter()
      .append("polygon")
        .attr("points", (oc: (OC, String), _: Int) => {
          val (x, y) = positionOfNotion(oc._1)
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
          val (x, y) = positionOfNotion(oc._1)
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
          val pos = positionOfNotion(eq.obsClass)._1
          if (pos < 0) pos - 5 else pos + 5
        })
        .attr("y", (eq: Spectrum.EquivalenceNotion[OC], _: Int) => positionOfNotion(eq.obsClass)._2 + 5 )
        .attr("text-anchor", (eq: Spectrum.EquivalenceNotion[OC], _: Int) =>
          if (positionOfNotion(eq.obsClass)._1 < 0) "end" else "start")
        .text((eq: Spectrum.EquivalenceNotion[OC], _: Int) => eq.name)

  }
}
