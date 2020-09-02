package de.bbisping.coupledsim.algo

import utest._
import de.bbisping.coupledsim.tool.control.StructureOperation
import de.bbisping.coupledsim.algo.cs.SchematicCoupledSimilarity
import de.bbisping.coupledsim.tool.model.NodeID
import de.bbisping.coupledsim.util.Relation

/**
 * Tests that all coupled simulation algorithms compute the same maximal coupled simulations
 * like SchematicCoupledSimilarity for the whole TestSample set.
 */

object CoupledSimulations extends TestSuite {
  
  AlgorithmLogging.loggingActive = false
  
  val cannonicalCSimulations = for {
    (slug, system) <- TestSamples.samples
    val rel = new SchematicCoupledSimilarity(system).compute()
  } yield (slug, system, rel)
  
  val simulationAlgos = List[StructureOperation.StructureRelationAnalyzer](
      new StructureOperation.FixedPointCoupledSimilarityAnalyzer,
      new StructureOperation.BasicSACoupledSimilarityAnalyzer,
      new StructureOperation.GameCoupledSimilarityAnalyzer,
      new StructureOperation.GameCoupledSimilarityPlainAnalyzer
  )
  
  def testAlgo()(implicit path: utest.framework.TestPath) = {
    val algoSlug = path.value.last
    for {
      algo <- simulationAlgos find (_.slug == algoSlug)
      (slug, system, cannonicalRel) <- cannonicalCSimulations
    } {
      val (rel, _) = algo.analyze(system)
      println(slug)
      val relDiffCannonical = rel filter { case (s1: NodeID, s2: NodeID) => !cannonicalRel(s1, s2) }
      assert(relDiffCannonical == Relation[NodeID]() )
      val cannonicalDiffRel = cannonicalRel filter { case (s1: NodeID, s2: NodeID) => !rel(s1, s2) }
      assert(cannonicalDiffRel == Relation[NodeID]() )
    }
  }
  
  val tests = Tests {
    "fixed-point-coupled-similarity" - testAlgo()
    //"basic-sa-coupled-similarity" - testAlgo() // broken!
    "game-coupled-similarity" - testAlgo()
    "game-coupled-similarity-plain" - testAlgo()
  }
}