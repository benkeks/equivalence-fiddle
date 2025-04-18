package io.equiv.eqfiddle.ts

import io.equiv.eqfiddle.util.LabeledRelation
import io.equiv.eqfiddle.util.Interpreting

import scala.collection.mutable.Queue


class CSVTSLoader(
    tsFileName: String
  ) {
  
  val silentActions = Set(Symbol("tau"), Symbol("i"))
  
  def result(): Option[WeakTransitionSystem[Int, Symbol, Unit]] = {

    val relationTuples = new Queue[(Int, Symbol, Int)]()
    val bufferedSource = scala.io.Source.fromFile(tsFileName)
    for (line <- bufferedSource.getLines) {
      val firstComma = line.indexWhere(_ == ',', 0)
      val secondComma = line.indexWhere(_ == ',', firstComma + 1)
      val start = line.slice(0, firstComma).trim
      val end = line.slice(firstComma + 1, secondComma).trim
      val label = line.slice(secondComma + 1, line.size).trim

      relationTuples += (( start.toInt, Symbol(label), end.toInt ))
    }
    bufferedSource.close

    val relation = new LabeledRelation(relationTuples.toSet)
    val nodeLabeling = (relation.lhs ++ relation.rhs).map((_, ())).toMap

    Some(new WeakTransitionSystem(relation, nodeLabeling, silentActions))
  }
}
