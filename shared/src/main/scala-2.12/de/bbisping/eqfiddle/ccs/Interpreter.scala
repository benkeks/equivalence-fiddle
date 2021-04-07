package de.bbisping.eqfiddle.ccs

import de.bbisping.eqfiddle.util.Interpreting
import de.bbisping.eqfiddle.util.Interpreting._
import de.bbisping.eqfiddle.util.Relation
import de.bbisping.eqfiddle.util.LabeledRelation
import de.bbisping.eqfiddle.ts.WeakTransitionSystem

/** Transforms a CCS term into a transition system */
class Interpreter[S, A, L](
    ccsDef: Syntax.Definition,
    stateIds: String => S,
    arrowLabeling: Option[Syntax.Label] => Interpreting.Result[A],
    nodeLabeling: Option[Syntax.NodeDeclaration] => Interpreting.Result[L],
    toInput: A => A,
    isOutput: A => Boolean
  ) {
  
  val silentActions = Set(arrowLabeling(Some(Syntax.Label("tau"))).get)

  val silentAction = silentActions.head
  
  val defaultFactory = new WeakTransitionSystem[S, A, L](_: LabeledRelation[S, A], _: Map[S, L], silentActions)

  val transitions = collection.mutable.Map[S, List[(A, S)]]()

  val sourceProcess = collection.mutable.Map[S, Syntax.ProcessExpression]()

  def result[R](factory: (LabeledRelation[S, A], Map[S, L]) => R = defaultFactory): Result[R] = {

    for {
      nodeDecls <- factorResults ( ccsDef.defs collect {
        case d: Syntax.NodeDeclaration => convertNodeDeclaration(d)
      } )
      procs <- factorResults ( ccsDef.defs collect {
        case Syntax.ProcessDeclaration(name, proc, _) =>
          convertProcess(proc) map { res =>
            println("define " + name + " = " + transitions(res))
            transitions(stateIds(name)) = transitions(res)
            sourceProcess(stateIds(name)) = proc
            (res, stateIds(name))
          }
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
      sourceProcess(stateId) = e
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
        case Syntax.Parallel(procs, _) =>
          println("interpreting parallel")
          val initialTransitions = for {
            p <- procs
            Success(s0) = convertProcess(p)
          } yield transitions(s0)

          println("initial trans " + initialTransitions)
          
          val initialTransitionsGrouped =
            initialTransitions.map(_.groupBy { case (a, _) => (toInput(a), isOutput(a)) }).zipWithIndex

          println("initial trans grouped " + initialTransitionsGrouped)

          val newSyncTransitions = for {
            (initsA, iA) <- initialTransitionsGrouped
            (initsB, iB) <- initialTransitionsGrouped
            if iA != iB
            bOutputs = initsB.filterKeys(_._2)
            ((actionA, _), succA) <- initsA.filterKeys(k => !k._2)
            sA <- succA
            sB <- bOutputs.getOrElse((actionA, true), List())
          } yield {
            val newProcs = procs.zipWithIndex.map { case (p, i) =>
              if (i == iA) {
                sourceProcess(sA._2)
              } else if (i == iB) {
                sourceProcess(sB._2)
              } else {
                p
              }
            }
            val Success(newState) = convertProcess(Syntax.Parallel(newProcs, e.position))
            println("synced:" + (silentAction, newState))
            (silentAction, newState)
          }
          println("synced:" + newSyncTransitions)
          val newInterleavedTransitions: List[(A, S)] = for {
            (initsA, iA) <- initialTransitions.zipWithIndex
            (a, s) <- initsA
          } yield {
            val newProcs = procs.updated(iA, sourceProcess(s))
            val Success(newState) = convertProcess(Syntax.Parallel(newProcs, e.position))
            println("interleaved:" + newProcs)
            (a, newState)
          }

          transitions(stateId) = newSyncTransitions ++ newInterleavedTransitions
        case Syntax.ProcessName(l, _) =>
          if (!transitions.isDefinedAt(stateId)) transitions(stateId) = List()
      }
    }
    Success(stateId)
  }
}
