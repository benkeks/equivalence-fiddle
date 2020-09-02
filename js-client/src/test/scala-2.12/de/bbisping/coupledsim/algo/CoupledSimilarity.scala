package de.bbisping.coupledsim.algo

import utest._
import de.bbisping.coupledsim.util.Relation
import de.bbisping.coupledsim.tool.control.StructureOperation
import de.bbisping.coupledsim.algo.cs.SchematicCoupledSimilarity
import de.bbisping.coupledsim.algo.transform.TauLoopCompression
import de.bbisping.coupledsim.tool.model.NodeID


/**
 * Tests that all coupled similarity algorithms compute the same maximal coupled simulations
 * like SchematicCoupledSimilarity for the whole TestSample set.
 * 
 * (the tests are run on systems with compressed tau cycles.)
 * 
 */

object CoupledSimilarity extends TestSuite {
  
  AlgorithmLogging.loggingActive = false
  
  val cannonicalCSimulationEqs = for {
    (slug, system) <- TestSamples.samples
    val systemWithOutTauCycles = new TauLoopCompression(system).compute()
    val rel = new SchematicCoupledSimilarity(systemWithOutTauCycles).compute()
  } yield (slug, systemWithOutTauCycles, rel.antisymmetricClosure)
  
  val simulationAlgos = List[StructureOperation.StructureAnalyzer](
      new StructureOperation.TauClosureCoupledSimilarityAnalyzer  
  )
  
  def testAlgo()(implicit path: utest.framework.TestPath) = {
    val algoSlug = path.value.last
    for {
      algo <- simulationAlgos find (_.slug == algoSlug)
      (slug, system, cannonicalRel) <- cannonicalCSimulationEqs
    } {
      println(slug)
      val coloring = algo.analyze(system)
      val rel = Relation.fromColoring(coloring)
      //assert(rel == cannonicalRel)
      
      val relDiffCannonical = rel filter { case (s1: NodeID, s2: NodeID) => !cannonicalRel(s1, s2) }
      assert(relDiffCannonical == Relation[NodeID]() )
      val cannonicalDiffRel = cannonicalRel filter { case (s1: NodeID, s2: NodeID) => !rel(s1, s2) }
      assert(cannonicalDiffRel == Relation[NodeID]() )
    }
  }
  
  val tests = Tests {
    "tau-closure-coupled-similarity" - testAlgo()
  }
}