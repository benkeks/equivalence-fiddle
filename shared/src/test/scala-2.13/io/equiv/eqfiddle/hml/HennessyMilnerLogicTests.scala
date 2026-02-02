package io.equiv.eqfiddle.hml

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should
import io.equiv.eqfiddle.hml.StrongObservationNotion.LTBTS

class HennessyMilnerLogicTests extends AnyFunSpec with should.Matchers  {

  import HML._

  describe("The HML metric") {
    val ob1 = Observe("a", True[String])
    it(ob1 + " should have coordinates (1,0,0,0,0,0)") {
      LTBTS.classifier(ob1) should equal (StrongObservationNotion(1,0,0,0,0,0))
    }
    
    val ob2 = Observe("a", And( Set[Formula[String]]( Negate( Observe("a", True[String])))))
    it(ob2 + " should have coordinates (2,1,0,0,1,1)") {
      LTBTS.classifier(ob2) should equal (StrongObservationNotion(2,1,0,0,1,1))
    }

    val ob3 = Observe("a", And( Set[Formula[String]](
      Negate( Observe("a", True[String])),
      Observe("a", True[String]),
      Observe("a", Observe("a", True[String]))
    )))
    it(ob3 + " should have coordinates (3,1,2,1,1,1)") {
      LTBTS.classifier(ob3) should equal (StrongObservationNotion(3,1,2,1,1,1))
    }

    val conj1a = And(Set[Formula[String]](
      Observe("a", Observe("a", True[String])),
      Observe("b", True[String])))
    val conj1b = And(Set[Formula[String]](
      Observe("a", Observe("a", True[String])),
      Observe("b", Observe("b", True[String]))))
    it(conj1a + " should be cheaper than " + conj1b) {
      (LTBTS.classifier(conj1a) < LTBTS.classifier(conj1b)) should be (true)
    }
  }

  describe("When selecting equivalences") {
    it("the best preorder if there are no distinctions should be bisimulation") {
      LTBTS.getStrongestPreorderClass(List()) should equal (List(LTBTS.getSpectrumClass("bisimulation")))
    }
  }
}