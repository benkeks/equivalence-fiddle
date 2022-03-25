package de.bbisping.eqfiddle.hml

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should

class HennessyMilnerLogicTests extends AnyFunSpec with should.Matchers  {

  import HennessyMilnerLogic._

  describe("The HML metric") {
    val ob1 = Observe("a", True[String])
    it(ob1 + " should have coordinates (1,0,0,0,0,0)") {
      ob1.obsClass should equal (ObservationClass(1,0,0,0,0,0))
    }
    
    val ob2 = Observe("a", Negate( Observe("a", True[String])))
    it(ob2 + " should have coordinates (2,1,0,0,1,1)") {
      ob2.obsClass should equal (ObservationClass(2,1,0,0,1,1))
    }

    val ob3 = Observe("a", And( Set[Formula[String]](
      Negate( Observe("a", True[String])),
      Observe("a", True[String]),
      Observe("a", Observe("a", True[String]))
    )))
    it(ob3 + " should have coordinates (3,1,1,2,1,1)") {
      ob3.obsClass should equal (ObservationClass(3,1,1,2,1,1))
    }

    val conj1a = And(Set[Formula[String]](
      Observe("a", Observe("a", True[String])),
      Observe("b", True[String])))
    val conj1b = And(Set[Formula[String]](
      Observe("a", Observe("a", True[String])),
      Observe("b", Observe("b", True[String]))))
    it(conj1a + " should be cheaper than " + conj1b) {
      (conj1a.obsClass strictlyBelow conj1b.obsClass) should be (true)
    }
  }


  describe("When selecting equivalences") {
    it("the best preorder if there are no distinctions should be bisimulation") {
      ObservationClass.getStrongestPreorderClass(List()) should equal (ObservationClass.getSpectrumClass("bisimulation").toList)
    }
  }
}