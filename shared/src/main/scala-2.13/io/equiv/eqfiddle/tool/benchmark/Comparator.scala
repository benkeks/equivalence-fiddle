package io.equiv.eqfiddle.tool.benchmark

import io.equiv.eqfiddle.algo.transform.TauLoopCompression
import io.equiv.eqfiddle.hml.HML
import io.equiv.eqfiddle.spectroscopy.Spectroscopy
import io.equiv.eqfiddle.ts.CSVTSLoader
import io.equiv.eqfiddle.ts.WeakTransitionSystem
import io.equiv.eqfiddle.util.LabeledRelation

class Comparator(
  algorithm: (WeakTransitionSystem[Int, Symbol, String]) => Spectroscopy[Int, Symbol, String, HML.Formula[Symbol]]
) {

  def run(
      fileName: String,
      leftInput: String,
      rightInput: String,
      config: Spectroscopy.Config,
      divergences: Boolean = false
    ): Unit = {
    val system = new CSVTSLoader(fileName).result().getOrElse(
      throw new IllegalArgumentException(s"Could not load transition system from '$fileName'.")
    )
    val left = resolveState(system, leftInput)
    val right = resolveState(system, rightInput)
    val comparedSystem = if (divergences) {
      val deadlock = system.nodes.find(state => system.post(state).isEmpty).getOrElse(
        throw new IllegalArgumentException("Transition system must have a deadlock state to mark divergence.")
      )
      val compressed = new TauLoopCompression(system, protectedNodes = Set(left, right)).compute()
      val markedTransitions = for {
        state <- compressed.nodes
        if compressed.silentSteps(state, state)
      } yield (state, Symbol("\u03b4"), deadlock)
      new WeakTransitionSystem(
        new LabeledRelation(compressed.step.tupleSet ++ markedTransitions),
        compressed.nodeLabeling,
        compressed.silentActions
      )
    } else system
    val comparedLeft = if (comparedSystem.nodes.contains(left)) left else right
    val comparedRight = if (comparedSystem.nodes.contains(right)) right else left
    val result = algorithm(comparedSystem).decideAll(Seq((comparedLeft, comparedRight)), config)

    println(result.foundPreorders(comparedLeft, comparedRight).map(_.name).mkString(", "))
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