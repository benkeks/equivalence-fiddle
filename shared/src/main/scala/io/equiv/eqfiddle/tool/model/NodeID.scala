package io.equiv.eqfiddle.tool.model

import io.equiv.eqfiddle.ts.TransitionSystem

class NodeID private (val name: String) extends Ordered[NodeID] {
  
  val hash = name.hashCode()
  
  override def hashCode() = hash
  
  override def equals(other: Any) = this eq other.asInstanceOf[AnyRef]
  
  def compare(that: NodeID): Int = this.hash - that.hash
  
  override def toString() = name
}

object NodeID {
  private val cache = collection.mutable.Map.empty[String, NodeID]
  
  // assumes single-threaded-ness for now
  def apply(name: String) = {
    cache getOrElseUpdate(name, new NodeID(name))
  }
  
  def freshNodeID(ts: TransitionSystem[NodeID, _, _], oldID: NodeID, suffixNum: Int = 0): NodeID = {
    val suggestedID = NodeID(oldID.name + "_" + suffixNum)
    if (ts.isFreshNode(suggestedID)) {
      suggestedID
    } else {
      freshNodeID(ts, oldID, suffixNum + 1)
    }
  }
}