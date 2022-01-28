package de.bbisping.eqfiddle.ts

import de.bbisping.eqfiddle.util.Relation

trait SilentActions[A] {
  val silentActions: Set[A]
}
