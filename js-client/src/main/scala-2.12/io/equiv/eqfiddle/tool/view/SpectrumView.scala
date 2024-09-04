package io.equiv.eqfiddle.tool.view

import scala.scalajs.js
import org.scalajs.dom
import org.singlespaced.d3js.d3
import js.JSConverters._
import org.singlespaced.d3js.Ops.fromFunction2To3DoublePrimitive
import org.singlespaced.d3js.Ops.fromFunction2To3StringPrimitive

import io.equiv.eqfiddle.hml.ObservationClass
import io.equiv.eqfiddle.hml.Spectrum

class SpectrumView[+OC <: ObservationClass](spectrum: Spectrum[OC], parentId: String) {
  val width = 500
  val height = 450

  val axes = List(
    (0,-10),
    (10,-15),
    (-5,-25),
    (50,-10),
    (-8,-13),
    (8,-13),
    (-10,-10),
    (10,-10),
    (-10,-10),
    (10,-15)
  )


  val svg = d3.select(parentId)
    .append("svg")
      .attr("width", width)
      .attr("height", height)
    .append("g")
      .attr("transform",
          s"translate(${width / 2}, $height)")

  render()

  def positionOfNotion(oc: ObservationClass): (Double, Double) = {
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
      val componentStrength = if (componentInt > 2) 2 else componentInt
      x = x + componentStrength * axis._1
      y = y + componentStrength * axis._2
    }

    (x, y)
  }

  def render() = {

    val dots = svg.append("g")
      .selectAll(".eq-notion-dot")
      .data(spectrum.notions.toJSArray)
      .enter()
      .append("circle")
        .attr("cx", (eq: Spectrum.EquivalenceNotion[OC], _: Int) => positionOfNotion(eq.obsClass)._1 )
        .attr("cy", (eq: Spectrum.EquivalenceNotion[OC], _: Int) => positionOfNotion(eq.obsClass)._2 )
        .attr("r", 2)

    val names = svg.append("g")
      .selectAll(".eq-notion-name")
      .data(spectrum.notions.toJSArray)
      .enter()
      .append("text")
        .attr("x", (eq: Spectrum.EquivalenceNotion[OC], _: Int) => {
          val pos = positionOfNotion(eq.obsClass)._1
          if (pos < 0) pos - 10 else pos + 10
        })
        .attr("y", (eq: Spectrum.EquivalenceNotion[OC], _: Int) => positionOfNotion(eq.obsClass)._2 )
        .attr("text-anchor", (eq: Spectrum.EquivalenceNotion[OC], _: Int) =>
          if (positionOfNotion(eq.obsClass)._1 < 0) "end" else "start")
        .text((eq: Spectrum.EquivalenceNotion[OC], _: Int) => eq.name)

  }
}
