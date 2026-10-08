package io.equiv.eqfiddle

import io.equiv.eqfiddle.ccs.CCSSamples
import io.equiv.eqfiddle.ccs.Example
import io.equiv.eqfiddle.spectroscopy.CCSSpectroscopy

object TestSamples {
  
  val samples = for {
    Example(slug, name, src) <- CCSSamples.namedSamples
  } yield (slug, CCSSpectroscopy.loadLTS(src).fold(error => throw new IllegalArgumentException(error.toString), identity))
  
}