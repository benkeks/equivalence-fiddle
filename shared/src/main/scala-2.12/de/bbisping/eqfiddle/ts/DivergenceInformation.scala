package de.bbisping.eqfiddle.ts

import de.bbisping.eqfiddle.util.Relation

trait DivergenceInformation[S] {
  
  def diverges(s: S): Boolean

}
