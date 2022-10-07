package de.bbisping.eqfiddle.hml

object Spectrum {
  case class EquivalenceNotion[+OC <: ObservationClass](name: String, obsClass: OC)

  def fromTuples[OC <: ObservationClass](pairs: List[(String, OC)], classifier: HennessyMilnerLogic.Formula[_] => OC) = {
    new Spectrum(pairs.map(p => EquivalenceNotion(p._1, p._2)), classifier)
  }
}

case class Spectrum[+OC <: ObservationClass](
    notions: List[Spectrum.EquivalenceNotion[OC]],
    classifier: HennessyMilnerLogic.Formula[_] => OC) {
  import Spectrum._

  /** given a group of least distinguishing observation classes, tell what weaker ObservationClasses would be the strongest fit to preorder the distinguished states */
  def getStrongestPreorderClass(leastClassifications: Iterable[EquivalenceNotion[ObservationClass]]): List[EquivalenceNotion[OC]] = {
    getStrongestPreorderClassFromClass(leastClassifications.map(_.obsClass))
  }

  def getStrongestPreorderClassFromClass(leastClassifications: Iterable[ObservationClass]): List[EquivalenceNotion[OC]] = {

    val weakerClasses = notions.filterNot { c => leastClassifications.exists(c.obsClass >= _) }
    val mostFitting = weakerClasses.filterNot { c => weakerClasses.exists(_.obsClass > c.obsClass) }

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
  def classifyClass(c: ObservationClass): List[EquivalenceNotion[OC]] = {
    val balancedClass = c // TODO maybe change?
    val classifications = notions.filter(en => (balancedClass lub en.obsClass) == en.obsClass)
    var currentMax = List[OC]()
    val leastClassifications = for {
      en <- classifications
      if !currentMax.exists(en.obsClass >= _)
    } yield {
      currentMax = en.obsClass :: currentMax
      en
    }
    leastClassifications
  }

  def classifyNicely(f: HennessyMilnerLogic.Formula[_]) = {
    val (_, classifications) = classifyFormula(f)
    classifications.map(_.name).mkString(",")
  }

  def getLeastDistinguishing[CF <: HennessyMilnerLogic.Formula[_]](formulas: Iterable[CF]): Iterable[CF] = {
    val classifications = formulas.map(f => (f, classifyFormula(f)))
    val allClassBounds = classifications.flatMap(_._2._2.map(_.obsClass))

    for {
      (f, (_, namedClassBounds)) <- classifications
      classBounds = namedClassBounds.map(_.obsClass)
      // just keep formulas where one of the classifications is dominated by no other classification
      if classBounds.exists(classBound => !allClassBounds.exists(_ < classBound))
    } yield f
  }
}