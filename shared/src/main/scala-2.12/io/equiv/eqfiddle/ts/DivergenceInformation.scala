package io.equiv.eqfiddle.ts

import io.equiv.eqfiddle.util.Relation

trait DivergenceInformation[S] {
  
  def diverges(s: S): Boolean

}
