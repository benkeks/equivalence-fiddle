package de.bbisping.coupledsim.ts

import de.bbisping.coupledsim.util.Relation

trait DivergenceInformation[S] {
  
  def diverges(s: S): Boolean

}
