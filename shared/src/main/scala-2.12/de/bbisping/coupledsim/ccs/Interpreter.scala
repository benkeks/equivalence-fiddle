package de.bbisping.coupledsim.ccs

import de.bbisping.coupledsim.util.Interpreting
import de.bbisping.coupledsim.util.Interpreting._
import de.bbisping.coupledsim.util.Relation
import de.bbisping.coupledsim.util.LabeledRelation
import de.bbisping.coupledsim.ts.WeakTransitionSystem

/** Transforms a CCS term into a transition system */
class Interpreter[S, A, L](
    ccsDef: Syntax.Definition,
    stateIds: String => S,
    arrowLabeling: Option[Syntax.Label] => Interpreting.Result[A],
    nodeLabeling: Option[Syntax.NodeDeclaration] => Interpreting.Result[L]
  ) {
  
  val silentActions = Set(arrowLabeling(Some(Syntax.Label("τ"))).get)
  
  val defaultFactory = new WeakTransitionSystem[S, A, L](_: LabeledRelation[S, A], _: Map[S, L], silentActions)

  val transitions = collection.mutable.Map[S, List[(A, S)]]()
    
  def result[R](factory: (LabeledRelation[S, A], Map[S, L]) => R = defaultFactory): Result[R] = {
    
    for {
      nodeDecls <- factorResults ( ccsDef.defs collect {
        case d: Syntax.NodeDeclaration => convertNodeDeclaration(d)
      } )
      procs <- factorResults ( ccsDef.defs collect {
        case Syntax.ProcessDeclaration(name, proc, _) =>
          convertProcess(proc) map ((_, stateIds(name)))
      } )
    } yield {
      val namedStates = procs.toMap.withDefault(x => x)
      val trans = for {
        (s0, as1) <- transitions.toSet
        (a, s1) <- as1 
      } yield (namedStates(s0), a, namedStates(s1))

      val labelMap = nodeDecls.toMap
      val relation = new LabeledRelation(trans).filterReachable(namedStates.values)
      val emptyLabels = (relation.lhs ++ relation.rhs) map { n => (n, nodeLabeling(None).get) } toMap
      val labels = emptyLabels ++ labelMap
      
      factory(relation, labels)
    }
    
  }
  
  private def convertNodeDeclaration(n: Syntax.NodeDeclaration): Result[(S, L)] = n match {
    case d @ Syntax.NodeDeclaration(name, attribs, pos) =>
      nodeLabeling(Some(d)).map((stateIds(name), _))
    case _ =>
      Problem("Node not accepted.", List(n))
  }
  
  private def convertProcess(e: Syntax.ProcessExpression): Result[S] = {
    val stateId = stateIds(e.toString)
    
    if (!transitions.isDefinedAt(stateId)) {
      e match {
        case Syntax.Prefix(l, proc, _) =>
          val Success(s0) = convertProcess(proc)
          val Success(a) = arrowLabeling(Some(l))
          transitions(stateId) = List((a, s0))
        case Syntax.Choice(procs, _) =>
          val newTransitions = for {
            p <- procs
            Success(s0) = convertProcess(p)
            (a, s1) <- transitions(s0)
          } yield (a, s1)
          transitions(stateId) = newTransitions
        case Syntax.ProcessName(l, _) =>
          if (!transitions.isDefinedAt(stateId)) transitions(stateId) = List()
      }
    }
    Success(stateId)
  }
}
