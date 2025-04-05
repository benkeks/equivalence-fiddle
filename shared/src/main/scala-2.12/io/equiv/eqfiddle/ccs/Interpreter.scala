package io.equiv.eqfiddle.ccs

import io.equiv.eqfiddle.util.Interpreting
import io.equiv.eqfiddle.util.Interpreting._
import io.equiv.eqfiddle.util.Parsing.Pos0
import io.equiv.eqfiddle.util.Relation
import io.equiv.eqfiddle.util.LabeledRelation
import io.equiv.eqfiddle.ts.WeakTransitionSystem
import io.equiv.eqfiddle.ccs.Syntax.Restrict
import io.equiv.eqfiddle.algo.AlgorithmLogging

import io.equiv.eqfiddle.algo.transform.WeakTransitionSaturation
import io.equiv.eqfiddle.algo.sigref.Bisimilarity
import io.equiv.eqfiddle.algo.sigref.BranchingBisimilarity
import io.equiv.eqfiddle.algo.transform.BuildQuotientSystem
import io.equiv.eqfiddle.algo.transform.DivergenceFinder

/** Transforms a CCS term into a transition system */
class Interpreter[S, A, L](
    ccsDef: Syntax.Definition,
    stateIds: String => S,
    arrowLabeling: Option[Syntax.Label] => Interpreting.Result[A],
    nodeLabeling: Option[Syntax.NodeAnnotation] => Interpreting.Result[L],
    toInput: A => A,
    isOutput: A => Boolean,
    maxStates: Int = 5000
  ) {
  
  val silentActions = Set(arrowLabeling(Some(Syntax.Label("τ"))).get, arrowLabeling(Some(Syntax.Label("tau"))).get)

  val silentAction = silentActions.head
  
  def defaultFactory(steps: LabeledRelation[S, A], labels: Map[S, L]) =
    (labels.keySet, new WeakTransitionSystem[S, A, L](steps, labels, silentActions))

  val transitions = collection.mutable.Map[S, List[(A, S)]]()
  private val todo = collection.mutable.Buffer[Syntax.ProcessExpression]()
  var stateCounter = 0

  def result(factory: (LabeledRelation[S, A], Map[S, L]) => (Set[S], WeakTransitionSystem[S, A, L]) = defaultFactory _): Result[WeakTransitionSystem[S, A, L]] = {
   
    val procEnv = ccsDef.defs collect {
      case d@Syntax.ProcessDefinition(name, proc, _) =>
        scheduleConversion(Syntax.ProcessName(Syntax.Label(name)))
        (name, proc)
    } toMap;

    // Check for unguarded recursion (up to level 4)
    val unguarded = procEnv.mapValues(_.unguardedNames).withDefaultValue(Set[String]())
    val unguarded4 = unguarded.mapValues(_.flatMap(unguarded(_))).mapValues(_.flatMap(unguarded(_))).mapValues(_.flatMap(unguarded(_)))
    val unguardedRecursion = for {
      (name, unguardedNames) <- unguarded4
      if unguardedNames contains name
    } yield {
      procEnv(name)
    }
    if (unguardedRecursion.nonEmpty) {
      return Problem("Unguarded recursion not allowed.", unguardedRecursion.toList)
    }

    // translate all processes
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
      if (stateCounter > maxStates) {
        return Problem(s"Too many states. Stopped translation at $stateCounter. (Check that you don't have accidental dynamic growth in parallel compositions!)", intialProcs)
      }
    }
    val mainNodes = ccsDef.defs collect {
      case d: Syntax.NodeAnnotation if d.attributeDefined("main") || d.attributeDefined("implicit-main") => d.name
    }
    val interestingNodes =
      if (mainNodes.isEmpty)
        procEnv.keys.toList
      else
        mainNodes
    val nodeAnnotations = ccsDef.defs collect {
      case d: Syntax.NodeAnnotation => d
    }
    // add dummy annotations for interesting nodes that lack any
    val completedAnnotations = nodeAnnotations ++ {
      for {
        node <- interestingNodes
        if !nodeAnnotations.exists(_.name == node)
      } yield {
        Syntax.NodeAnnotation(node, List(), Pos0)
      }
    }
      
    for {
      nodeDecls <- factorResults ( completedAnnotations collect {
        case d @ Syntax.NodeAnnotation(name, attribs, pos) =>
          if (mainNodes.isEmpty && procEnv.isDefinedAt(name)) {
            convertNodeAnnotation(Syntax.NodeAnnotation(name, attribs :+ ("implicit-main", ""), pos))
          } else {
            convertNodeAnnotation(d)
          }
      })
    } yield {
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

      val metaSettings = for {
        (name, attribsNested) <- ccsDef.defs collect {
          case Syntax.MetaDeclaration(name, attribs, _) => (name, attribs) 
        } groupBy(_._1)
        attribs = attribsNested.map(_._2).flatten
      } yield {
        (name, attribs)
      }

      val (mainConcreteNodes, genTs) = factory(relation, labels)
      var ts = genTs

      val preprocessing = metaSettings.getOrElse("preprocessing", List[String]())
      for (
        method <- preprocessing
      ) {
        method match {
          case "weakness_saturated" =>
            ts = new WeakTransitionSaturation[S, A, L](ts).compute()
          case "bisim_minimized" =>
            val bisimColoring = new Bisimilarity[S, A, L](ts).computePartition()
            ts = new BuildQuotientSystem[S, A, L](ts, bisimColoring, mainConcreteNodes).build()
          case "srbb_minimized" =>
            val divergenceInfo = new DivergenceFinder[S, A, L](ts).compute()
            val bisimColoring = new BranchingBisimilarity[S, A, L](
              ts, stabilityRespecting = true
            ).computePartition()
            ts = new BuildQuotientSystem[S, A, L](ts, bisimColoring, protectedNodes = mainConcreteNodes, tauCyclesOn = Some(divergenceInfo)).build()
        }
      }
      ts
    }
  }
  
  private def convertNodeAnnotation(n: Syntax.NodeAnnotation): Result[(S, L)] = n match {
    case d @ Syntax.NodeAnnotation(name, attribs, pos) =>
      nodeLabeling(Some(d)).map((stateIds(name), _))
    case _ =>
      Problem("Node not accepted.", List(n))
  }

  private def scheduleConversion(e: Syntax.ProcessExpression): S = {
    val stateId = stateIds(e.toString)
    if (!transitions.isDefinedAt(stateId)) {
      todo += e
      stateCounter += 1
      if (stateCounter % 100 == 0) {
        AlgorithmLogging.debugLog(s"State count: $stateCounter", logLevel = 8)
      }
      if (todo.size >= 400 && todo.size % 100 == 0) {
        AlgorithmLogging.debugLog(s"Todo length: ${todo.size}", logLevel = 5)
      }
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
        p match {
          case Restrict(namesC, procC, posC) =>
            (a, Syntax.Restrict(names ++ (namesC.filterNot(names.contains(_))), procC, pos) )
          case other =>
            (a, Syntax.Restrict(names, p, pos))
        }
      }
    case Syntax.Renaming(rns, proc, pos) =>
      val aRns = rns.map { case (from, to) => (arrowLabeling(Some(from)).get, arrowLabeling(Some(to)).get) }
      for {
        (a, p) <- semantics(procEnv)(proc)
      } yield {
        val action = aRns.find(_._1 == a).map(_._2).getOrElse(a)
        //p match {
          // case Syntax.Renaming(rnsC, procC, posC) =>
          //   // NOTE: This would be a hack that only makes sense for hiding.
          //   (action, Syntax.Renaming(rns ++ (rnsC.filterNot { case (from, to) => rns.contains(from) } ), procC, pos) )
          // case other =>
        (action, Syntax.Renaming(rns, p, pos))
        // }
      }
    case Syntax.ProcessName(l, pos) =>
      val continuation = procEnv.getOrElse(
        l.name,
        Syntax.Prefix(l, Syntax.NullProcess(pos), pos)
      )
      if (continuation.unguardedNames().contains(l.name)) {
        // direct loop
        AlgorithmLogging.debugLog(s"Unguarded recursion at ${e.position.line}", logLevel = 4)
        List()
      } else {
        semantics(procEnv)(continuation)
      }
  }
}
