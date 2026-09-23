package io.equiv.eqfiddle.tool.benchmark

import io.equiv.eqfiddle.hml.HML
import io.equiv.eqfiddle.spectroscopy.Spectroscopy
import io.equiv.eqfiddle.ts.CSVTSLoader
import io.equiv.eqfiddle.ts.WeakTransitionSystem

class Comparator(
  algorithm: (WeakTransitionSystem[Int, Symbol, String]) => Spectroscopy[Int, Symbol, String, HML.Formula[Symbol]]
) {

  def run(
      fileName: String,
      leftInput: String,
      rightInput: String,
      config: Spectroscopy.Config
    ): Unit = {
    val system = new CSVTSLoader(fileName).result().getOrElse(
      throw new IllegalArgumentException(s"Could not load transition system from '$fileName'.")
    )
    val left = resolveState(system, leftInput)
    val right = resolveState(system, rightInput)
    val result = algorithm(system).decideAll(Seq((left, right)), config)

    println(result.foundPreorders(left, right).map(_.name).mkString(", "))
  }

  private def resolveState(
      system: WeakTransitionSystem[Int, Symbol, String],
      input: String
    ): Int = {
    input.toIntOption match {
      case Some(id) =>
        require(system.nodes.contains(id), s"No state with id $id exists.")
        id
      case None =>
        system.nodesByLabel.get(input) match {
          case Some(ids) if ids.size == 1 => ids.head
          case Some(_) => throw new IllegalArgumentException(s"State label '$input' is ambiguous.")
          case None => throw new IllegalArgumentException(s"No state with label '$input' exists.")
        }
    }
  }
}