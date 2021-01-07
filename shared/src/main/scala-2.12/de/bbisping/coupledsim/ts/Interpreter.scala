package de.bbisping.coupledsim.ts

import de.bbisping.coupledsim.util.Interpreting
import de.bbisping.coupledsim.util.Interpreting._
import de.bbisping.coupledsim.util.Relation
import de.bbisping.coupledsim.tool.model.NodeID
import de.bbisping.coupledsim.util.LabeledRelation

class Interpreter[S, A, L](
    tsDef: Syntax.Definition,
    stateIds: String => S,
    arrowLabeling: Option[Syntax.Label] => Interpreting.Result[A],
    nodeLabeling: Option[Syntax.NodeDeclaration] => Interpreting.Result[L]
  ) {
  
  import TransitionSystem._
  
  val defaultFactory = TransitionSystem[S, A, L](_, _)
  
  def result[R](factory: (LabeledRelation[S, A], Map[S, L]) => R = defaultFactory): Result[R] = {
    
    for {
      nodeDecls <- factorResults ( tsDef.defs collect {
        case d: Syntax.NodeDeclaration => convertNodeDeclaration(d)
      } )
      rels <- factorResults ( tsDef.defs collect {
        case r: Syntax.Relation => convertRelation(r)
      } )
      labelMap = nodeDecls.toMap
      trans = new LabeledRelation(rels.toSet)
      emptyLabels = ((trans.lhs ++ trans.rhs) map { n => (n, nodeLabeling(None).get) } toMap)
      labels = emptyLabels ++ labelMap
    } yield factory(trans, labels)
    
  }
  
  private def convertNodeDeclaration(n: Syntax.NodeDeclaration): Result[(S, L)] = n match {
    case d @ Syntax.NodeDeclaration(name, attribs, pos) =>
      nodeLabeling(Some(d)).map((stateIds(name), _))
    case _ =>
      Problem("Node not accepted.", List(n))
  }
  
  private def convertRelation(e: Syntax.Relation): Result[(S, A, S)] = e match {
    case Syntax.StepTo(Syntax.SingleNode(n1, _), aL @ Syntax.Label(l, _), Syntax.SingleNode(n2, _), _) => 
      arrowLabeling(Some(aL)).map((stateIds(n1), _, stateIds(n2)))
    case e =>
      Problem("Illegal Relation.", List(e))
  }
}
