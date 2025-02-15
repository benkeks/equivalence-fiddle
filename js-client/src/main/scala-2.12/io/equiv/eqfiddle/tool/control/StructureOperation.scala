package io.equiv.eqfiddle.tool.control

trait StructureOperation {

  val name: String

  val slug: String

  val category: String = "default"

  val description: String = ""

  def applyOperation(system: Structure): Unit

  override def toString() = slug
}