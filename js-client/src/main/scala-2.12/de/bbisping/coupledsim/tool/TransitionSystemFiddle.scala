package de.bbisping.coupledsim.tool

import scala.scalajs.js.JSApp
import scala.scalajs.js.annotation.{JSExportTopLevel, JSExport}
import de.bbisping.coupledsim.tool.arch.Action
import de.bbisping.coupledsim.tool.arch.ActionDispatcher
import de.bbisping.coupledsim.tool.arch.Control
import de.bbisping.coupledsim.ccs.CCSSamples
import de.bbisping.coupledsim.tool.control.Source
import de.bbisping.coupledsim.tool.control.Structure
import de.bbisping.coupledsim.tool.view.GraphRenderer
import de.bbisping.coupledsim.tool.view.SourceEditor
import de.bbisping.coupledsim.tool.control.Pipeline

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