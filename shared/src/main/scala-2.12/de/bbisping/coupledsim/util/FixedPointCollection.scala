package de.bbisping.coupledsim.util

class FixedPointCollection[A](step: (Iterable[A]) => Iterable[A]) {
  def apply(initial: Iterable[A]): Iterable[A] = {
    val newValues = step(initial)
    if (newValues.isEmpty) {
      initial
    } else {
      initial ++ apply(newValues)
    }
  }
}

object FixedPointCollection {
  def apply[A](step: (Iterable[A]) => Iterable[A]): FixedPointCollection[A] =
    new FixedPointCollection(step)
}