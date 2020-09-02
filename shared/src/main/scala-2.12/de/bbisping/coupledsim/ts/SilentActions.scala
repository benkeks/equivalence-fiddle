package de.bbisping.coupledsim.ts

import de.bbisping.coupledsim.util.Relation

trait SilentActions[A] {
  val silentActions: Set[A]
}
