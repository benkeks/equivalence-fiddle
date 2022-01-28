package de.bbisping.eqfiddle.ts

import de.bbisping.eqfiddle.util.LabeledRelation
import de.bbisping.eqfiddle.util.Relation
import scala.collection.mutable.LinkedList
import scala.collection.mutable.Seq
import scala.collection.mutable.Queue

class WeakTransitionSystem[S, A, L](
    step: LabeledRelation[S, A],
    nodeLabeling: Map[S, L],
    override val silentActions: Set[A])
  extends TransitionSystem[S, A, L](step, nodeLabeling)
    with SilentActions[A] {

  def this(ts: TransitionSystem[S, A, L], silentActions: Set[A]) {
    this(ts.step, ts.nodeLabeling, silentActions)
  }

  val silentSteps = {
    step filter {
      case (p1, a, p2) => silentActions(a)
    } toRelation
  }

  import scala.collection.mutable.HashMap
  import scala.collection.mutable.HashSet
  private val silentReachableCache = new HashMap[S, Set[S]]

  def silentReachableCached(s: S): Set[S] = {
    silentReachableCache.getOrElseUpdate(s, {
      computeSilentReachableCached(s)
    })
  }

  private def computeSilentReachableCached(s: S): Set[S] = {
    val visited: HashSet[S] = HashSet()
    val todo = Queue[S](s)

    while (todo.nonEmpty) {
      val a = todo.dequeue()
      if (!visited(a)) {
        visited += a
        silentReachableCache.get(a) match {
          case None =>
            todo ++= silentSteps.values(a)
          case Some(reach) =>
            visited ++= reach
        }
      }
    }

    visited.toSet
  }

  //val silentReachable = (silentSteps.transitiveClosureFast.reflexiveClosureOn(nodes)) rep

  def silentReachable(s: S): Set[S] = silentReachableCached(s)

  val silentReachableInverse = Map[S, Set[S]]()//new Relation(silentReachable).inverse.rep

  def tauMaximalNode(s: S) = {
    step.rep(s) exists { case (a, ss) =>
      silentActions(a) && ss.nonEmpty
    }
  }

  def weakEnabled(s: S) = {
    for {
      s1 <- silentReachable(s)
      a <- enabled(s1)
      if !silentActions(a)
    } yield a
  }

  def weakEnabled2(s: S) = {
    for {
      s1 <- silentReachable(s)
      a <- enabled(s1)
    } yield a
  }

  def weakReachingActions(s: S) = {
    for {
      s1 <- silentReachableInverse(s)
      a <- reachingActions(s1)
      if !silentActions(a)
    } yield a
  }

  def weakPost(s: S, a: A): Set[S] = {
    val ss1 = silentReachable(s)
    if (silentActions(a)) {
      ss1
    } else {
      for {
        s2 <- post(ss1, a)
        s3 <- silentReachable(s2)
      } yield s3
    }
  }

  def weakPostDelay(s: S, a: A): Set[S] = {
    val ss1 = silentReachable(s)
    if (silentActions(a)) {
      ss1
    } else {
      for {
        s2 <- post(ss1, a)
      } yield s2
    }
  }

  def weakPost(s: S): Map[A, Set[S]] = {
    for {
      a <- weakEnabled2(s)
    } yield (a, weakPost(s, a))
  } toMap

  def weakPre(s: S, a: A): Set[S] = {
    val ss1 = silentReachableInverse(s)
    if (silentActions(a)) {
      ss1
    } else {
      for {
        s2 <- pre(ss1, a)
        s3 <- silentReachableInverse(s2)
      } yield s3
    }
  }

  def weakPre(s: S): Map[A, Set[S]] = {
    {
      for {
        a <- weakReachingActions(s)
      } yield (a, weakPre(s, a))
    }.toMap
  }

  def weakPre(ss: Set[S]): Map[A, Set[S]] = {
    for { s <- ss; sp <- weakPre(s) toList } yield sp
  }.groupBy(_._1).mapValues(_.flatMap(_._2))

}