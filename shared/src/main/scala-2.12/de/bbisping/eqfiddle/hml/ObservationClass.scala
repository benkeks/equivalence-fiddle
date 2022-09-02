package de.bbisping.eqfiddle.hml

trait ObservationClass extends PartiallyOrdered[ObservationClass] {

  def lub[B >: ObservationClass](that: B): B

  def glb[B >: ObservationClass](that: B): B

  def toTuple: Product
}

object ObservationClass {
}
