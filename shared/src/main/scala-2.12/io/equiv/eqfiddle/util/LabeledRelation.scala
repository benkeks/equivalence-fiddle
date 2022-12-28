package io.equiv.eqfiddle.util

import scala.collection.immutable.Map
import scala.collection.immutable.Set

/**
 * Represent E×L×E relations using curried Maps
 */
class LabeledRelation[E, L](val rep: Map[E, Map[L, Set[E]]]) {
 
  /*{(1,1,3), {1,1,1}} ~> {(1 |-> 1 |-> {1,3})}*/
  def this(tuples: Set[(E, L ,E)]) {
    this(tuples.groupBy(_._1).mapValues(_.groupBy(_._2).mapValues(_.map(_._3))))
  }
  
  def size = rep.values.map(_.values.map(_.size).sum).sum
  
  def apply(e1: E, l: L, e2: E) = {
    rep.getOrElse(e1, Map()).getOrElse(l, Set())(e2)
  }
  
  def apply(e1le2: (E, L, E)) = e1le2 match {
    case (e1, l, e2) =>
      rep.getOrElse(e1, Map()).getOrElse(l, Set())(e2)
  }
  
  /**
   * for e1 returns the set containing all the e2 such that R(e1, l, e2)
   */
  def values(e1: E, l: L) = {
    rep.getOrElse(e1, Map()).getOrElse(l, Set())
  }
  
  /**
   * for e2 returns the set containing all the e1 such that e1 R e2
   */
  def valuesInverse(e2: E, l: L) = {
    inverseRep.getOrElse(e2, Map()).getOrElse(l, Set())
  }
  
  def +(e1: E, l: L, e2: E) = {
    val mapE1 = rep.getOrElse(e1, Map())
    val setL = mapE1.getOrElse(l, Set())
    val newRep = rep.updated(e1, mapE1.updated(l, setL + e2))
    
    new LabeledRelation(newRep)
  }
  
  /*{(1 |-> 1 |-> {1,2})} ~> {(1,1,2), {1,1,1}} */
  lazy val tupleSet: Set[(E,L,E)] = {
    for {
      (e1, lee2) <- rep
      (l, ee2) <- lee2
      e2 <- ee2
    } yield (e1, l, e2)
  }.toSet
  
  lazy val inverseRep = {
    val invTuples = tupleSet.map { case (e1, l, e2) => (e2, l, e1) }
    invTuples.groupBy(_._1).mapValues(_.groupBy(_._2).mapValues(_.map(_._3)))
  }
  
  lazy val lhs = rep.keySet
  lazy val labels = rep.values.flatMap(_.keySet)
  lazy val rhs = rep.values.flatMap(_.values).flatten.toSet

  def merge(other: LabeledRelation[E, L]) =
    new LabeledRelation(tupleSet ++ other.tupleSet)
  
  def remove(other: LabeledRelation[E, L]) =
    new LabeledRelation(tupleSet.filterNot(other(_)))
  
  def filter(f: (E, L, E) => Boolean) =
    new LabeledRelation(tupleSet.filter { case (e1, l, e2) => f(e1, l, e2) })
  
  def toRelation = 
    new Relation(tupleSet map { case (e1, l, e2) => (e1, e2) })
  
  def determinize(pick: Iterable[(L, E)] => (L, E)): LabeledRelation[E, L] = {
    val pickedTuples = for {
      (e1, lee2) <- rep
    } yield {
      val succs = for {
        (l, ee2) <-lee2.to
        e2 <- ee2
      } yield (l, e2)
      val (lP, e2P) = pick(succs)

      (e1, lP, e2P)
    }

    new LabeledRelation[E, L](pickedTuples.toSet)
  }
  
  def peekPath(e: E): List[E] = {
    rep.get(e).flatMap(_.headOption.flatMap{ case (l, ee2) => ee2.headOption.map(e :: peekPath(_)) }).getOrElse(List())
  }

/*  def removeKeys(toRemove: Set[E]) = {
    new Relation(tupleSet.filterNot {
      case  (l, r) => (toRemove contains l) || (toRemove contains r)
    })
  }*/
  /*
  def transitiveClosure = {
    val comp = FixedPoint[Map[E,Set[E]]](
        {rel => rel.mapValues{
          r => r ++ r.flatMap(rel.getOrElse(_, Set()))}},
        {case (a,b) => a == b})
    
    new Relation(comp(rep))
  }*/
  
  def isTransitive = rep.forall {
    case (e1,lee2) => lee2 forall {
      case (l, ee2) =>
        ee2.flatMap(values(_, l)).forall { e3 =>
          apply(e1, l, e3)
        }
    }
  }
  
  def symmetricClosure = {
    new LabeledRelation(tupleSet ++ tupleSet.map { case (e1, l, e2) => (e2, l, e1) })
  }

  def symmetricReflexiveClosure(reflexiveOn: Iterable[E], reflexiveEdge: L) = {
    new LabeledRelation(tupleSet ++ tupleSet.map { case (e1, l, e2) => (e2, l, e1) } ++ reflexiveOn.map(e => (e, reflexiveEdge, e)))
  }

  def isSymmetric = tupleSet.forall {
    case (e1, l, e2) => tupleSet contains (e2, l, e1)
  }

  def getReachablePart(startingSet: Iterable[E]) = {
    val visited = collection.mutable.Set[E]()
    val queue = collection.mutable.Queue[E]()
    queue ++= startingSet

    while (queue.nonEmpty) {
      val e = queue.dequeue()

      if (!visited(e)) {
        visited += e
      
        for (lee2 <- rep.get(e); ee2 <- lee2.values) {
          queue ++= ee2
        }
      }
    }

    visited.toSet
  }

  def filterReachable(startingSet: Iterable[E]) = {
    val reachable = getReachablePart(startingSet)
    filter {
      case (e1, _, _) => reachable(e1)
    }
  }
  
  def isReflexiveSomewhere = tupleSet.exists{case (e1, _, e2) => e1 == e2}

  /** If relation (restricted to selected labels) is an equivalence relation, outputs an equivalent "coloring" of E */
  def toQuotientColoring(labelFilter: L => Boolean, representative: (E, E) => E): Coloring[E] = {
    val colors = lhs.groupBy { e1 =>
      val post = for {
        (l, ee2) <- rep(e1)
        if labelFilter(l)
      } yield ee2.reduce(representative)
      post.fold(e1)(representative) // if the relation is irreflexive, a loop will be assumed anyways
    }
    Coloring.fromPartition(colors.values)
  }

  def toGraphString() = {
    val list = (lhs ++ rhs).toIndexedSeq
    val idFor = list.indices.map(i => (list(i), i)).toMap
    tupleSet.map { case (e1, l, e2) =>
      val label = l.toString()
      idFor(e1) + "->" + idFor(e2) + (if (label != "") "[label=\"" + label + "\"]" else "")
    }.mkString("digraph rel{\n  ", ";\n  ", "}")
  }
  
  def toCsvString() = {
    val list = (lhs ++ rhs).toIndexedSeq
    val idFor = list.indices.map(i => (list(i), i)).toMap
    tupleSet.map { case (e1, l, e2) =>
      val label = l.toString()
      idFor(e1) + "," + idFor(e2) + "," + label
    }.mkString("", "\n", "")
  }
  
  override def toString = tupleSet.mkString("{", ",", "}")
  
  override def equals(that: Any) = that match {
    case t: LabeledRelation[E, L] => rep == t.rep
    case _ => false
  }

  override def hashCode = rep.hashCode
}

object LabeledRelation {
  def apply[E, L](tuples: (E,L,E)*) = new LabeledRelation[E,L](tuples.toSet)
}