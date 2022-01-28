package de.bbisping.eqfiddle.ts

import de.bbisping.eqfiddle.util.LabeledRelation

case class TransitionSystem[S, A, L](
    val step: LabeledRelation[S, A],
    val nodeLabeling: Map[S, L]) {
  
  def post(s: S, a: A): Set[S] =
    step.values(s, a)
    
  def post(ss: Set[S], a: A): Set[S] =
    for { s <- ss; sp <- post(s, a) } yield sp
    
  def post(ss: Set[S]): Map[A, Set[S]] = {
    val en = for { s <- ss; a <- enabled(s) } yield a
    val p = for { a <- en } yield (a, post(ss, a))
    p.toMap
  }
  
  def post(s: S, aa: Set[A]): Set[S] =
    for { a <- aa; sp <- post(s, a) } yield sp
    
  def post(s: S) =
    step.rep.getOrElse(s, Map())
    
  def pre(s: S, a: A): Set[S] =
    step.valuesInverse(s, a)
    
  def pre(s: S): Map[A, Set[S]] =
    step.inverseRep.getOrElse(s, Map())
  
  def pre(ss: Set[S], a: A): Set[S] =
    for { s <- ss; sp <- pre(s, a) } yield sp
    
  def pre(ss: Set[S]): Map[A, Set[S]] = {
    for { s <- ss; sp <- pre(s) toList } yield sp
  }.groupBy(_._1).mapValues(_.flatMap(_._2))
  
  def enabled(s: S) = post(s).keySet
  
  def reachingActions(s: S) = pre(s).keySet
  
  val nodes = nodeLabeling.keySet
  
  val nodesByLabel =
    nodeLabeling.groupBy(_._2).mapValues(_.keySet)
    
  val actions = step.labels
    
  def isFreshNode(s: S) = !nodes.contains(s)
}