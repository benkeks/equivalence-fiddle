package de.bbisping.eqfiddle.util

import scala.collection.mutable.Builder

class FixedPoint[A](step: (A) => A, goodEnough: (A,A) => Boolean) {
  def apply(initial: A): A = {
    val newValue = step(initial)
    if (goodEnough(initial, newValue)) {
      newValue
    } else {
      apply(newValue)
    }
  }
  
  def applyIfNecessary(initial: A): A = {
    if (goodEnough(initial, initial)) {
      initial
    } else {
      apply(initial)
    }
  }
}

object FixedPoint {
  def apply[A](step: (A) => A, goodEnough: (A,A) => Boolean): FixedPoint[A] =
    new FixedPoint(step, goodEnough)
}