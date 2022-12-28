package io.equiv.eqfiddle.ccs

import io.equiv.eqfiddle.util.Interpreting
import io.equiv.eqfiddle.util.Interpreting._
import io.equiv.eqfiddle.util.Relation
import io.equiv.eqfiddle.util.LabeledRelation
import io.equiv.eqfiddle.ts.WeakTransitionSystem
import io.equiv.eqfiddle.ccs.Syntax.Restrict

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
  private val todo = collection.mutable.Buffer[Syntax.ProcessExpression]()

  def result[R](factory: (LabeledRelation[S, A], Map[S, L]) => R = defaultFactory): Result[R] = {

    val procEnv = ccsDef.defs collect {
      case d@Syntax.ProcessDeclaration(name, proc, _) =>
        scheduleConversion(Syntax.ProcessName(Syntax.Label(name)))
        (name, proc)
    } toMap;

    val intialProcs = todo.toList

    while (todo.nonEmpty) {
      val proc0 = todo.remove(0)
      val state0 = stateIds(proc0.toString)
      if (!transitions.isDefinedAt(state0)) {
        transitions(state0) = for {
          (a, proc1) <- semantics(procEnv)(proc0)
        } yield {
          (a, scheduleConversion(proc1))
        }
      }
    }

    for {
      nodeDecls <- factorResults ( ccsDef.defs collect {
        case d: Syntax.NodeDeclaration => convertNodeDeclaration(d)
      } )
    } yield {
      val mainNodes = ccsDef.defs collect {
        case d: Syntax.NodeDeclaration if d.attributeDefined("main") => d.name
      }
      val interestingNodes =
        if (mainNodes.isEmpty)
          procEnv.keys.toList
        else
          mainNodes

      val trans = for {
        (s0, as1) <- transitions.toSet
        (a, s1) <- as1
      } yield (s0, a, s1)

      val labelMap = nodeDecls.toMap
      val relation = new LabeledRelation(trans).filterReachable(interestingNodes.map(stateIds(_)))
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

  private def scheduleConversion(e: Syntax.ProcessExpression): S = {
    val stateId = stateIds(e.toString)
    if (!transitions.isDefinedAt(stateId)) {
      todo += e
    }
    stateId
  }

  private def semantics(procEnv: Map[String,Syntax.ProcessExpression])(e: Syntax.ProcessExpression): List[(A, Syntax.ProcessExpression)] = e match {
    case Syntax.Prefix(l, proc, _) =>
      val Success(a) = arrowLabeling(Some(l))
      List((a, proc))
    case Syntax.Choice(procs, _) =>
      procs.flatMap(semantics(procEnv)(_))
    case Syntax.Parallel(procs, _) =>
      val initialSteps = procs.map(semantics(procEnv)(_))
      val initialStepsGrouped =
        initialSteps.map(_.groupBy { case (a, _) => (toInput(a), isOutput(a)) }).zipWithIndex
      val newSyncSteps = for {
        (initsA, iA) <- initialStepsGrouped
        (initsB, iB) <- initialStepsGrouped.filterNot(_._2 == iA)
        bOutputs = initsB.filterKeys(_._2)
        ((actionA, _), succA) <- initsA.filterKeys(k => !k._2).toList
        pA <- succA
        pB <- bOutputs.getOrElse((actionA, true), Nil)
      } yield {
        val newProcs = procs.zipWithIndex.map { case (p, i) =>
          if (i == iA) {
            pA._2
          } else if (i == iB) {
            pB._2
          } else {
            p
          }
        }
        (silentAction, Syntax.Parallel(newProcs, e.position))
      }
      val newInterleavedSteps: List[(A, Syntax.ProcessExpression)] = for {
        (initsA, iA) <- initialSteps.zipWithIndex
        (a, p) <- initsA
      } yield {
        val newProcs = procs.updated(iA, p)
        (a, Syntax.Parallel(newProcs, e.position))
      }
      newSyncSteps ++ newInterleavedSteps
    case Restrict(names, proc, pos) =>
      val aNames = names.map(a => arrowLabeling(Some(a)).get)
      for {
        (a, p) <- semantics(procEnv)(proc)
        if !aNames.contains(toInput(a))
      } yield {
        (a, Syntax.Restrict(names, p, pos))
      }
    case Syntax.ProcessName(l, pos) =>
      semantics(procEnv)(
        procEnv.getOrElse(
          l.name,
          Syntax.Prefix(l, Syntax.NullProcess(pos), pos))
      )
  }
}
