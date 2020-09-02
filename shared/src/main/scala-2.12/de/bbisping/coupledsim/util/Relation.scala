package de.bbisping.coupledsim.util

import scala.collection.immutable.Map
import scala.collection.immutable.Set
import scalaz.Scalaz._

/**
 * Represent relations using Maps
 */
class Relation[E](val rep: Map[E, Set[E]]) {

  /*{(1,2), {1,1}} ~> {(1 |-> {1,2})}*/
  def this(tuples: Set[(E, E)]) {
    this(tuples.groupBy(_._1).mapValues(_.map(_._2)))
  }

  def size = rep.values.map(_.size).sum

  def apply(e1: E, e2: E) = {
    rep.getOrElse(e1, Set())(e2)
  }

  def apply(e1e2: (E, E)) = e1e2 match {
    case (e1, e2) =>
      rep.getOrElse(e1, Set())(e2)
  }

  /**
   * for e1 returns the set containing all the e2 such that e1 R e2
   */
  def values(e1: E) = {
    rep.getOrElse(e1, Set())
  }

  /**
   * for e2 returns the set containing all the e1 such that e1 R e2
   */
  def valuesInverse(e2: E) = {
    inverseRep.getOrElse(e2, Set())
  }

  /*{(1 |-> {1,2})} ~> {(1,2), {1,1}} */
  lazy val tupleSet: Set[(E, E)] = rep.toSet[(E, Set[E])].flatMap {
    case (l, rr) => rr.map((l, _))
  }

  lazy val inverseRep = tupleSet.map(_.swap).groupBy(_._1).mapValues(_.map(_._2))

  lazy val lhs = rep.keySet
  lazy val rhs = rep.values.flatten.toSet

  def merge(other: Relation[E]) = new Relation(rep |+| other.rep)

  def +(e1: E, e2: E) = new Relation(rep |+| Map(e1 -> Set(e2)))

  def -(e1: E, e2: E) = new Relation(rep updated (e1, rep.getOrElse(e1, Set()) - e2))

  def filter(f: (E, E) => Boolean) = new Relation(tupleSet.filter { case (e1, e2) => f(e1, e2) })

  def remove(other: Relation[E]) = new Relation(tupleSet.filterNot(other(_)))

  def removeKeys(toRemove: Set[E]) = {
    new Relation(tupleSet.filterNot {
      case (l, r) => (toRemove contains l) || (toRemove contains r)
    })
  }

  def transitiveClosure = {
    val comp = FixedPoint[Map[E, Set[E]]](
      { rel =>
        rel.mapValues {
          r => r ++ r.flatMap(rel.getOrElse(_, Set()))
        }
      },
      { case (a, b) => a == b })

    new Relation(comp(rep))
  }
  
  def transitiveClosureFast = {
    import scala.collection.mutable.MultiMap
    import scala.collection.mutable.HashMap
    import scala.collection.mutable.HashSet
    
    // warshall's algorithm (not extremely fast but faster than naive fix point ;) )
    val nodes = lhs ++ rhs
    
    val connections = HashSet(tupleSet.toSeq: _*)
    
    for {
      a <- nodes
      b <- nodes
      c <- nodes
      if connections((b,a)) && connections((a,c)) 
    } {
      connections += ((b,c))
    }
    
    new Relation(connections.toSet)
  }

  def isTransitive = rep.forall {
    case (a, bb) => bb.flatMap(values).forall {
      c => apply(a, c)
    }
  }

  def symmetricClosure = {
    new Relation(tupleSet ++ tupleSet.map(_.swap))
  }

  def isSymmetric = tupleSet.forall {
    case (l, r) => tupleSet contains (r, l)
  }

  def antisymmetricClosure = {
    new Relation(tupleSet filter { case (r, l) => apply(l, r) })
  }

  def isReflexiveSomewhere = tupleSet.exists { case (l, r) => l == r }

  def isReflexive = (lhs ++ rhs).forall(v => apply(v, v))

  def reflexiveClosureOn(nodes: Set[E]) = {
    new Relation(tupleSet ++ (nodes map (e => (e, e))))
  }
  
  def inverse = new Relation(inverseRep)

  def getPathsFrom(start: E): Set[List[E]] = getPathsFrom(Set(start))

  def getPathsFrom(start: Set[E]): Set[List[E]] = {
    val comp = FixedPointCollection[List[E]] { paths =>
      paths.flatMap { p =>
        values(p.last).
          map(next => p :+ next) // all one step extensions from its end
      }
    }
    comp(start map (e => List(e))).toSet
  }

  def flattenLattice() = {
    val maxElements = for {
      e <- rhs
      ve = values(e)
      if ve.isEmpty || ve == Set(e)
    } yield e

    val newRep = rep.mapValues(_ filter maxElements)

    new Relation(newRep)
  }

  def toGraphString(edgeLabeling: (E, E) => String) = {
    val list = (lhs ++ rhs).toIndexedSeq
    val idFor = list.indices.map(i => (list(i), i)).toMap
    tupleSet.map {
      case (l, r) =>
        val label = edgeLabeling(l, r)
        idFor(l) + "->" + idFor(r) + (if (label != "") "[label=\"" + label + "\"]" else "")
    }.mkString("digraph rel{\n  ", ";\n  ", "}")
  }

  override def toString = tupleSet.mkString("{", ",", "}")

  override def equals(that: Any) = that match {
    case t: Relation[E] => rep == t.rep
    case _              => false
  }

  override def hashCode = rep.hashCode
}

object Relation {
  def apply[E](tuples: (E, E)*) = new Relation[E](tuples.toSet)

  def fromColoringPartitionRelation[E](part: Coloring[E], rel: Relation[Coloring.Color]) = {
    val partitions = part.partitions.withDefaultValue(Set[E]())
    val newRel = for {
      (l, r) <- rel.tupleSet
      l1 <- partitions(l)
      r1 <- partitions(r)
    } yield (l1, r1)
    new Relation(newRel)
  }

  def fromColoring[E](part: Coloring[E]) = {
    val partitions = part.partitions.withDefaultValue(Set[E]())
    val newRel = for {
      p <- partitions.keySet
      val partMembers = partitions(p)
      l1 <- partMembers
      r1 <- partMembers
    } yield (l1, r1)
    new Relation(newRel)
  }
}