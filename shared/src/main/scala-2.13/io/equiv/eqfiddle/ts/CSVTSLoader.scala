package io.equiv.eqfiddle.ts

import io.equiv.eqfiddle.util.LabeledRelation
import io.equiv.eqfiddle.util.Interpreting

import scala.collection.mutable.Queue


class CSVTSLoader(
    tsFileName: String
  ) {
  
  val silentActions = Set(Symbol("tau"), Symbol("i"), Symbol("τ"))
  
  def result(): Option[WeakTransitionSystem[Int, Symbol, String]] = {

    val relationTuples = new Queue[(Int, Symbol, Int)]()
    val labelingTuples = new Queue[(Int, String)]()
    val bufferedSource = scala.io.Source.fromFile(tsFileName)
    for (line <- bufferedSource.getLines()) {
      val firstComma = line.indexWhere(_ == ',', 0)
      val secondComma = line.indexWhere(_ == ',', firstComma + 1)
      val start = line.slice(0, firstComma).trim
      val end = line.slice(firstComma + 1, secondComma).trim
      var label = line.slice(secondComma + 1, line.size).trim
      if (label.startsWith("\"")) {
        // remove enclosing quotes
        label = label.substring(1, label.length - 1)
      }
      end.toIntOption match {
        case Some(tarId) =>
          relationTuples += (( start.toInt, Symbol(label), end.toInt ))
        case None => // second parameter is a label, not a node id
          // collect node names, but drop additional node meta info (third parameter)
          labelingTuples += (( start.toInt, end ))
      }
    }
    bufferedSource.close

    val relation = new LabeledRelation(relationTuples.toSet)
    val tempNodeLabeling = labelingTuples.toMap
    val nodeLabeling = (relation.lhs ++ relation.rhs).map(id => (id, tempNodeLabeling.get(id).getOrElse(""))).toMap

    Some(new WeakTransitionSystem(relation, nodeLabeling, silentActions))
  }
}
