package de.bbisping.eqfiddle.util

import scala.collection.mutable.HashMap

case class Coloring[E](val colors: Map[E, Coloring.Color] = Map()) {
  
  lazy val universe = colors.values.toSet

  def universeSize() = universe.size
  
  lazy val partitions = 
    colors.toSet[(E, Coloring.Color)].groupBy(_._2).mapValues(_.map(_._1))
  
  def freshColor() = universe.max + 1
  
  def apply(e: E) = colors.apply(e)

  def get(e: E) = colors.get(e)
  
  def partitionFor(e: E) = {
    for {
      color <- colors.get(e)
    } yield {
      for {
        (e, c) <- colors
        if c == color
      } yield e
    }.toSet
  }
  
  def map(f: (E, Coloring.Color) => Coloring.Color) = {
    Coloring(colors map { case (e, c) => (e, f(e, c)) })
  }
  
  def filter(f: E => Boolean) = {
    Coloring(colors.filterKeys(f))
  }
  
  def split(otherPart: Set[E]) = {
    val offSet = freshColor()
    
    val newColors = for {
      (e, c) <- colors
      if (otherPart contains e) 
    } yield (e, offSet + c)
    
    Coloring(colors ++ newColors)
  }
  
  def splitInsertColors[A](other: Iterable[(E, A)])  = {
    // following the sort+partition approach of [BRR2016]
    var colorOffset = freshColor()
    val newColors = new HashMap[E, Coloring.Color]()
    
    val updates = for {
      ((a, ea), i) <- other.groupBy(_._2).zipWithIndex
      (e, _) <- ea
    } {
      newColors(e) = colorOffset + i
    }
    
    Coloring(colors ++ newColors)
//    var currentColorOffset = freshColor()
//    
//    val updates = for {
//      cols <- other.groupBy(_._2).values
//    } yield {
//      val newCols = for ((e, c) <- cols) yield {
//        val newC = c + currentColorOffset
//        (e, newC)
//      }
//      currentColorOffset = newCols.map(_._2).max + 1
//      newCols.toIterable
//    }
//    
//    Coloring(colors ++ updates.flatten)
  }
  
  def representativeFor(e: E)(implicit cmp: Ordering[E]) = {
    partitionFor(e).map(_.max)
  }
  
  /** normalizes the color space to 0..n-1 for n colors, where the color order is the order of the partition representatives*/
  def normalize()(implicit cmp: Ordering[E]) = {
    
    val colorMap = partitions.toList.sortBy(_._2.max).map(_._1).zipWithIndex.toMap
    Coloring(colors.mapValues(colorMap))
  }
  
  /** normalizes the color space to 0..n-1 for n colors, where the color order is the old color order*/
  def normalizeReturnMap() = {
    
    val colorMap = partitions.toList.sortBy(_._1).map(_._1).zipWithIndex.toMap
    (Coloring(colors.mapValues(colorMap)), colorMap)
  }
  
  def toStringPretty()(implicit cmp: Ordering[E]) = {
    colors.toList.sorted.mkString("Coloring { ", ", ", "}" )
  }
  
  def toPartition() = {
    partitions
  }
}

object Coloring {
  
  type Color = Int
  
  def fromPartition[E](sets: Iterable[Set[E]]) = {
    val c = for {
      (s, idx) <- sets.zipWithIndex
      e <- s
    } yield (e, idx)
    Coloring(c.toMap)
  }
  
}