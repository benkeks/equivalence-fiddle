package de.bbisping.coupledsim.util

case class Partition[E](val parts: Set[Set[E]]) {
  
  lazy val universe = parts.flatten
  
  def partitionFor(e: E) = {
    parts find ( _ contains e )
  }
  
  /** creates a new partition, by subtracting all the stuff another (smaller) partition introduces and
   *  merging the remainder with the new partition. */
  def split(other: Partition[E]) = {
    val oU = other.universe
    val shrunkenParts = for {
      p <- parts
      nP = p filterNot oU
      if nP.nonEmpty
    } yield nP
    Partition(other.parts ++ shrunkenParts)
  }
  
  def splitInsert(part: Set[E]) = {
    val shrunkenParts1 = for {
      p <- parts
      val nP = p -- part
      if nP.nonEmpty
    } yield nP
    val shrunkenParts2 = for {
      p <- parts
      val nP = p intersect part
      if nP.nonEmpty
    } yield nP
    
    Partition(shrunkenParts1 ++ shrunkenParts2)
  }
  
  def representativeFor(e: E)(implicit cmp: Ordering[E]) = {
    partitionFor(e).map(_.max)
  }
}