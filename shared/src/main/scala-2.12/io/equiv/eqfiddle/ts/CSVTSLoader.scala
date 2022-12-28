package io.equiv.eqfiddle.ts

import io.equiv.eqfiddle.util.LabeledRelation
import io.equiv.eqfiddle.util.Interpreting

import scala.collection.mutable.Queue


class CSVTSLoader(
    tsFileName: String
  ) {
  
  val silentActions = Set(Symbol("tau"))
  
  def result(): Option[WeakTransitionSystem[Int, Symbol, Unit]] = {

    val relationTuples = new Queue[(Int, Symbol, Int)]()
    val bufferedSource = scala.io.Source.fromFile(tsFileName)
    for (line <- bufferedSource.getLines) {
      val trans = line.split(",").map(_.trim)
      relationTuples += (( trans(0).toInt, Symbol(trans(2)), trans(1).toInt ))
    }
    bufferedSource.close

    val relation = new LabeledRelation(relationTuples.toSet)
    val nodeLabeling = (relation.lhs ++ relation.rhs).map((_, ())).toMap

    Some(new WeakTransitionSystem(relation, nodeLabeling, silentActions))
  }
}
