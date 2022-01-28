package de.bbisping.eqfiddle


import de.bbisping.eqfiddle.ts.DirectTSImporter
import scala.io.Source

object VLTSSamples  {
  
  val srcFiles = Map(               //     S,      T,   tau-T,  a
      "vasy_0_1" -> "vasy_0_1.csv", //   289,   1224,  no tau,  2
      "vasy_1_4" -> "vasy_1_4.csv", //  1183,   4464,    1213,  6
      "cwi_1_2" -> "cwi_1_2.csv"    //  1952,   2387,    2215, 26
//      "cwi_3_14" -> "cwi_3_14.csv"  //  3996,  14552,   14551,  2
  )
  
  def getSampleFile(slug: String) = "shared/src/test/assets/vlts/" + srcFiles(slug)
  
  def getSample(slug: String) = {
    
    val srcContent = Source.fromFile(getSampleFile(slug)).getLines().mkString("\n")
    
    new DirectTSImporter(srcContent).result()
  }
  
}