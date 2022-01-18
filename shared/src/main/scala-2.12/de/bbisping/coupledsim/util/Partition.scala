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
      nP = p -- part
      if nP.nonEmpty
    } yield nP
    val shrunkenParts2 = for {
      p <- parts
      nP = p intersect part
      if nP.nonEmpty
    } yield nP
    
    Partition(shrunkenParts1 ++ shrunkenParts2)
  }
  
  def representativeFor(e: E)(implicit cmp: Ordering[E]) = {
    partitionFor(e).map(_.max)
  }
}

object Partition {

  def partitioningListsOfSet[E](ee: Set[E]): Iterable[List[Set[E]]] = {
    if (ee.isEmpty) {
      List(List())
    } else {
      val focused = ee.head
      val rest = ee.tail
      if (rest.isEmpty) {
        List(List(ee))
      } else {
        val restPartitionings = partitioningListsOfSet(rest)
        for {
          partition <- restPartitionings
          i <- (0 to partition.length)
        } yield {
          if (i == partition.length) {
            partition :+ Set(focused)
          } else {
            partition.updated(i, partition(i) + focused)
          }
        }
      }
    }
  }

}