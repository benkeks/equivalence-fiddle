package de.bbisping.eqfiddle.hml

trait ObservationClass extends PartiallyOrdered[ObservationClass] {

  def lub[B >: ObservationClass](that: B): B

  def glb[B >: ObservationClass](that: B): B

  def toTuple: Product
}

object ObservationClass {

  trait ClassifiedFormula[A] extends HennessyMilnerLogic.Formula[A] {

    def obsClass: ObservationClass

    /** class of this formula if it appears at the top level */
    def getRootClass(): ObservationClass

  }
}
