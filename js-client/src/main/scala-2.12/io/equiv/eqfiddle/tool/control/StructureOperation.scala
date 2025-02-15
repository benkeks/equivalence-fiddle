package io.equiv.eqfiddle.tool.control

import scala.scalajs.js.Date

import io.equiv.eqfiddle.algo.AlgorithmLogging
import io.equiv.eqfiddle.algo.transform.NaiveDivergenceFinder
import io.equiv.eqfiddle.algo.transform.NaiveReflexiveClosure
import io.equiv.eqfiddle.algo.transform.NaiveTransitiveClosure
import io.equiv.eqfiddle.algo.transform.WeakStepAdder
import io.equiv.eqfiddle.algo.sigref
import io.equiv.eqfiddle.algo.checkers.NaiveCoupledSimCheck
import io.equiv.eqfiddle.tool.control.Structure.ActionLabel
import io.equiv.eqfiddle.tool.model.NodeID
import io.equiv.eqfiddle.ts.DivergenceInformation
import io.equiv.eqfiddle.ts.TransitionSystem
import io.equiv.eqfiddle.ts.WeakTransitionSystem
import io.equiv.eqfiddle.util.Coloring
import io.equiv.eqfiddle.util.LabeledRelation
import io.equiv.eqfiddle.util.Relation
import io.equiv.eqfiddle.algo.transform.TauLoopCompression
import io.equiv.eqfiddle.algo.transform.BuildQuotientSystem
import io.equiv.eqfiddle.algo.transform.InternalizedNondeterminismSaturation

trait StructureOperation {

  val name: String

  val slug: String

  val category: String = "default"

  val description: String = ""

  def applyOperation(system: Structure): Unit

  override def toString() = slug
}

object StructureOperation {

  /** Some analyzer which returns a coloring/partition of nodes. */
  trait StructureAnalyzer extends StructureOperation {

    def analyze(system: Structure.TSStructure): Coloring[NodeID]

    /** calls the analyzer and sets the current partition for the structure. */
    override def applyOperation(structure: Structure): Unit = {
      val begin = Date.now
      val c = analyze(structure.structure)
      println("Analyzer «" + name + "» took: " + (Date.now - begin) + "ms.")
      structure.setPartition(c)
    }
  }

  /** Some analyzer which returns a relation on nodes. */
  trait StructureRelationAnalyzer extends StructureOperation {

    def analyze(system: Structure.TSStructure): (Relation[NodeID], Option[AlgorithmLogging[NodeID]])

    /** calls the analyzer and sets the current relation for the structure. */
    override def applyOperation(structure: Structure): Unit = {
      val begin = Date.now
      val (r, log) = analyze(structure.structure)
      println("Analyzer «" + name + "» took: " + (Date.now - begin) + "ms.")
      structure.setRelation(r)
      for {l <- log} structure.setReplay(l.getReplay())
    }
  }

  class SigrefBisimilarityAnalyzer extends StructureRelationAnalyzer {

    override val name = "Sigref Bisimilarity"

    override val slug = "sigref-bisimilarity"

    override val category = "SimBisim"

    override val description =
      "Computes the strong bisimilarity relation " +
      "by iterating a signature refinement algorithm [Wim11]."

    override def analyze(system: Structure.TSStructure) = {
      val algo = new sigref.Bisimilarity(system)
      (algo.compute(), Some(algo))
    }
  }

  class SigrefWeakBisimilarityAnalyzer extends StructureRelationAnalyzer {

    override val name = "Sigref Weak Bisimilarity"

    override val slug = "sigref-weak-bisimilarity"

    override val category = "SimBisim"

    override val description =
      "Computes the weak bisimilarity relation " +
      "by iterating a signature refinement algorithm [Wim11]."

    override def analyze(system: Structure.TSStructure) = {
      val algo = new sigref.WeakBisimilarity(system)
      (algo.compute(), Some(algo))
    }
  }

  trait StructureTransformer extends StructureOperation {

    def transform(system: Structure.TSStructure): Structure.TSStructure

    override def applyOperation(structure: Structure) = {
      val begin = Date.now
      val c = transform(structure.structure)
      println("Transformer «" + name + "» took: " + (Date.now - begin) + "ms.")
      structure.setStructure(c)
    }
  }

  class NaiveDivergenceMarker extends StructureTransformer {

    override val name = "Naive Divergence Marker"

    override val slug = "naive-divergence-marker"

    override def transform(system: Structure.TSStructure): Structure.TSStructure = {
      val coloring = new NaiveDivergenceFinder(system).compute()

      new WeakTransitionSystem(system.step, system.nodeLabeling, system.silentActions) with DivergenceInformation[NodeID] {
        def diverges(s: NodeID): Boolean =
          coloring.colors(s) == 1
      }
    }
  }

  class NaiveTransitiveTauClosure extends StructureTransformer {

    override val name = "Transitive Tau Closure"

    override val slug = "naive-trans-tau-closure"

    override val category = "Closures"

    override val description =
      "Adds transitive closure of tau-steps."

    override def transform(system: Structure.TSStructure) = {
      val trans = new NaiveTransitiveClosure(system, system.silentActions).compute()
      Structure.transitionSystemConstructor(trans, system.nodeLabeling, Some(system))
    }
  }

  class NaiveReflexiveTauClosure extends StructureTransformer {

    override val name = "Reflexive Tau Closure"

    override val slug = "naive-refl-tau-closure"

    override val category = "Closures"

    override val description =
      "Adds reflexive closure of tau-steps."

    override def transform(system: Structure.TSStructure) = {
      val newTrans = new NaiveReflexiveClosure(system, Structure.silentActionLabel).compute()
      Structure.transitionSystemConstructor(newTrans.step, newTrans.nodeLabeling, Some(system))
    }
  }


  class WeakStepRelationClosure extends StructureTransformer {

    override val name = "Weak Step Relation Closure"

    override val slug = "weak-step-relation-closure"

    override val category = "Closures"

    override val description =
      "Adds visible steps after transitive tau-closure."

    override def transform(system: Structure.TSStructure) = {
      val trans = new WeakStepAdder(system, system.silentActions).compute()
      Structure.transitionSystemConstructor(trans, system.nodeLabeling, Some(system))
    }
  }
  
  class InternalizedNondeterminismSaturater extends StructureTransformer {

    override val name = "Internalized Nondeterminism Saturater"

    override val slug = "internalized-nondeterminism-saturater"

    override val category = "Closures"

    override val description =
      "TODO"

    override def transform(system: Structure.TSStructure): Structure.TSStructure = {

      var stateId = 0
      def newState(s: NodeID) = {
        stateId += 1
        NodeID.freshNodeID(system, NodeID(s.name + "__ndpostpone__"), stateId)
      }

      new InternalizedNondeterminismSaturation(system, newState).compute()
    }
  }


  class TauLoopCompressor extends StructureTransformer {

    override val name = "Tau-loop Compression"

    override val slug = "tau-loop-compression"

    override val category = "Compressions"

    override val description =
      "Compresses tau loops to tau cycles (Sec 4.1.1 of the thesis)."

    override def transform(system: Structure.TSStructure) = {
      new TauLoopCompression(system).compute()
    }
  }

  class QuotientBuilder extends StructureOperation {

    override val name = "Quotient Builder"

    override val slug = "quotient-builder"

    override val category = "Compressions"

    override val description =
      "Compresses a transition system to a quotient system based on the current coloring (Sec 4.1.2 of the thesis)."

    override def applyOperation(structure: Structure) = {
      val begin = Date.now
      val c = new BuildQuotientSystem(structure.structure, structure.partition).build()
      println("Transformer «" + name + "» took: " + (Date.now - begin) + "ms.")
      structure.setStructure(c)
    }
  }

}
