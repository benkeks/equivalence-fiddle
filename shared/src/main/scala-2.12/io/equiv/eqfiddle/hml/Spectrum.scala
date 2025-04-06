package io.equiv.eqfiddle.hml

object Spectrum {
  case class EquivalenceNotion[+OC <: ObservationNotion](name: String, obsNotion: OC)

  def fromTuples[OC <: ObservationNotion](pairs: List[(String, OC)], classifier: HennessyMilnerLogic.Formula[_] => OC) = {
    new Spectrum(pairs.map(p => EquivalenceNotion(p._1, p._2)), classifier)
  }
}

case class Spectrum[+OC <: ObservationNotion](
    notions: List[Spectrum.EquivalenceNotion[OC]],
    classifier: HennessyMilnerLogic.Formula[_] => OC) {
  import Spectrum._

  /** given a group of least distinguishing observation classes, tell what weaker ObservationNotiones would be the strongest fit to preorder the distinguished states */
  def getStrongestPreorderClass(leastClassifications: Iterable[EquivalenceNotion[ObservationNotion]]): List[EquivalenceNotion[OC]] = {
    getStrongestPreorderClassFromClass(leastClassifications.map(_.obsNotion))
  }

  def getStrongestPreorderClassFromClass(leastClassifications: Iterable[ObservationNotion]): List[EquivalenceNotion[OC]] = {

    val weakerClasses = notions.filterNot { c => leastClassifications.exists(c.obsNotion >= _) }
    val mostFitting = weakerClasses.filterNot { c => weakerClasses.exists(_.obsNotion > c.obsNotion) }

    mostFitting.toList
  }

  val getSpectrumClass = notions.map(en => (en.name, en)).toMap

  val notionNames = getSpectrumClass.keySet

  /** names the coarsest notion of equivalence where this formula is part of the distinguishing formulas */
  def classifyFormula[CF <: HennessyMilnerLogic.Formula[_]](f: CF): (OC, List[EquivalenceNotion[OC]]) = {
    val balancedClass = classifier(f)
    (balancedClass, classifyClass(balancedClass))
  }


  /** names the coarsest notion of equivalence where this classification is part of the distinguishing capacities */
  def classifyClass(c: ObservationNotion): List[EquivalenceNotion[OC]] = {
    val balancedClass = c // TODO maybe change?
    val classifications = notions.filter(en => (balancedClass lub en.obsNotion) == en.obsNotion)
    var currentMax = List[OC]()
    val leastClassifications = for {
      en <- classifications
      if !currentMax.exists(en.obsNotion >= _)
    } yield {
      currentMax = en.obsNotion :: currentMax
      en
    }
    leastClassifications
  }

  def classifyNicely(f: HennessyMilnerLogic.Formula[_]) = {
    val (_, classifications) = classifyFormula(f)
    classifications.map(_.name).mkString(",")
  }

  def selectCheapest[CF <: HennessyMilnerLogic.Formula[_]](formulas: Iterable[CF]): Iterable[CF] = {
    val classifications = formulas.map(f => (f, classifyFormula(f)))
    val allClassBounds = classifications.flatMap(_._2._2.map(_.obsNotion))

    for {
      (f, (_, namedClassBounds)) <- classifications
      classBounds = namedClassBounds.map(_.obsNotion)
      // just keep formulas where one of the classifications is dominated by no other classification
      if classBounds.exists(classBound => !allClassBounds.exists(_ < classBound))
    } yield f
  }
}