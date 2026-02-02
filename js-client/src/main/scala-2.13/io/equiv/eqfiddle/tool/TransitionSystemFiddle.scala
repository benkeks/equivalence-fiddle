package io.equiv.eqfiddle.tool

import scala.scalajs.js
import scala.scalajs.js.annotation.{JSImport, JSExportTopLevel, JSExport}
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
  
  lazy val renderer = new GraphRenderer(this)
  val editor = new SourceEditor(this)

  @JSExport
  def main(): Unit = {
    JSImportGlobals.ensureGlobals()
    source.init()
    structure.init()
    val initialCodeParameter = editor.getURLSampleSlug()
    val initialCode =
      CCSSamples.getExample(initialCodeParameter)
      .map(_.code)
      .getOrElse {
        // if the url parameter does not name an example, try to decode it as a code snippet
        if (initialCodeParameter.startsWith("code=")) 
          new String(java.util.Base64.getUrlDecoder().decode(initialCodeParameter.substring(5)))
        else
          CCSSamples.default
      }
    doAction(Source.LoadDefinition(initialCode), source)
  }

  def getActionTarget(a: Action) = a match {
    case _: Source.SourceAction => source
    case _: Structure.StructureAction => structure
    case _: Pipeline.PipelineAction => pipeline
  }
}

private object JSImportGlobals {
  @JSImport("d3", JSImport.Namespace)
  @js.native object D3Module extends js.Object

  @JSImport("jquery", JSImport.Default)
  @js.native object JQueryModule extends js.Object

  @JSImport("codemirror", JSImport.Default)
  @js.native object CodeMirrorModule extends js.Object

  @JSImport("bootstrap", JSImport.Namespace)
  @js.native object BootstrapModule extends js.Object

  def ensureGlobals(): Unit = {
    js.Dynamic.global.d3 = D3Module
    js.Dynamic.global.jQuery = JQueryModule
    js.Dynamic.global.$ = JQueryModule
    js.Dynamic.global.CodeMirror = CodeMirrorModule
    val _ = BootstrapModule
  }
}