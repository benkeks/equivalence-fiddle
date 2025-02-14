package io.equiv.eqfiddle.hml

trait ObservationNotion extends PartiallyOrdered[ObservationNotion] {

  def lub[B >: ObservationNotion](that: B): B

  def glb[B >: ObservationNotion](that: B): B

  def toTuple: Product
}

object ObservationNotion {
}
