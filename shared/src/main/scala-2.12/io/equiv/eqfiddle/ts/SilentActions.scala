package io.equiv.eqfiddle.ts

import io.equiv.eqfiddle.util.Relation

trait SilentActions[A] {
  val silentActions: Set[A]
}
