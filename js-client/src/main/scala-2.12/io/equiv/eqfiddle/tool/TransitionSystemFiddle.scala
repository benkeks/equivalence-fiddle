package io.equiv.eqfiddle.tool

import scala.scalajs.js.annotation.{JSExportTopLevel, JSExport}
import io.equiv.eqfiddle.tool.arch.Action
import io.equiv.eqfiddle.tool.arch.ActionDispatcher
import io.equiv.eqfiddle.tool.arch.Control
import io.equiv.eqfiddle.ccs.CCSSamples
import io.equiv.eqfiddle.tool.control.Source
import io.equiv.eqfiddle.tool.control.Structure
import io.equiv.eqfiddle.tool.view.GraphRenderer
import io.equiv.eqfiddle.tool.view.SourceEditor
import io.equiv.eqfiddle.tool.control.Pipeline

@JSExportTopLevel("TransitionSystemFiddle")
object TransitionSystemFiddle extends Control with ActionDispatcher {

  val source = new Source(this)
  val structure = new Structure(this)
  val pipeline = new Pipeline(this)
  
  val renderer = new GraphRenderer(this)
  val editor = new SourceEditor(this)

  @JSExport
  def main(): Unit = {
    source.init()
    structure.init()
    val initialCode =
      CCSSamples.getExample(editor.getURLSampleSlug())
      .map(_.code)
      .getOrElse(CCSSamples.default)
    doAction(Source.LoadDefinition(initialCode), source)
  }

  def getActionTarget(a: Action) = a match {
    case _: Source.SourceAction => source
    case _: Structure.StructureAction => structure
    case _: Pipeline.PipelineAction => pipeline
  }
}