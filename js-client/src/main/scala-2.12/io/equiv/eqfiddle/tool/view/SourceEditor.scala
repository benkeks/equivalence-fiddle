package io.equiv.eqfiddle.tool.view

import scala.scalajs.js
import scala.scalajs.js.Function0
import scala.scalajs.js.Any.fromFunction0
import scala.scalajs.js.Any.fromFunction1
import scala.scalajs.js.Any.fromInt
import scala.scalajs.js.Any.jsArrayOps
import scala.scalajs.js.Any.wrapArray
import scala.scalajs.js.URIUtils
import scala.scalajs.js.|.from
import org.scalajs.jquery.jQuery
import org.denigma.codemirror.CodeMirror
import org.denigma.codemirror.Editor
import org.denigma.codemirror.extensions.EditorConfig
import org.scalajs.dom
import org.scalajs.dom.raw.Event
import org.scalajs.dom.raw.EventTarget
import org.scalajs.dom.raw.HTMLElement
import org.scalajs.dom.raw.HTMLInputElement
import org.scalajs.dom.raw.HTMLTextAreaElement
import org.scalajs.dom.raw.SVGSVGElement
import org.scalajs.dom.raw.UIEvent
import org.singlespaced.d3js.Ops.fromFunction1To2
import org.singlespaced.d3js.Ops.fromFunction1To3
import org.singlespaced.d3js.Ops.fromFunction2To3
import org.singlespaced.d3js.d3
import io.equiv.eqfiddle.tool.arch.Control
import io.equiv.eqfiddle.tool.control.ModelComponent
import io.equiv.eqfiddle.ts.Samples
import io.equiv.eqfiddle.tool.control.Source
import io.equiv.eqfiddle.tool.control.Structure
import io.equiv.eqfiddle.tool.control.StructureOperation
import org.scalajs.jquery.JQueryEventObject
import io.equiv.eqfiddle.tool.control.Pipeline
import io.equiv.eqfiddle.algo.AlgorithmLogging
import io.equiv.eqfiddle.tool.model.NodeID
import org.denigma.codemirror.LineWidget
import io.equiv.eqfiddle.ccs.Syntax.MetaDeclaration

class SourceEditor(val main: Control) extends ViewComponent {
  val PROBLEM_GUTTER = "es-problem-gutter"
  
  var name = "transition_system"
  
  val editorNode = d3.select("#es-definition").node match {
      case el: HTMLTextAreaElement =>
        el
      case _ =>
        println("Cannot find text area for definition display -- creating own fall back text area!")
        val newNode = dom.document.createElement("textarea")
        dom.document.body.appendChild(newNode)
        newNode.asInstanceOf[HTMLTextAreaElement]
    }
  
  val editor = {
    val cfg = EditorConfig
        .mode("dces")
        .lineNumbers(true)
        .gutters(js.Array("CodeMirror-linenumbers", PROBLEM_GUTTER))
    CodeMirror.fromTextArea(editorNode, cfg)
  }
  
  // document used to input the source
  val sourceDoc = editor.getDoc
  sourceDoc.setValue("")
  
  // document to display the simulation state
  val pipelineDoc = sourceDoc.copy(false)
  pipelineDoc.setValue("")
  
  var lastText = ""
  var lastPipelineText = ""
  
  var runners = List[(String, List[String], Int)]()
  
  var currentPipelineLine = 0
  var pipelineReplayWidget: Option[LineWidget] = None
  
  var changeParseTimeout: Option[Int] = None
  
  var currentStructure: Option[Structure.TSStructure] = None

  editor.on("change", { (_: Editor) =>
    changeParseTimeout foreach dom.window.clearTimeout
    changeParseTimeout = Some(dom.window.setTimeout(() => onChange(), 400))
  }: js.Function1[Editor, Unit])
  
  editor.on("gutterClick", { (_: Editor, line: Int) =>
    triggerLineAction(line)
  }: js.Function2[Editor, Int, Unit])

  d3.select("#es-load-file").node().asInstanceOf[HTMLInputElement].onchange = onLoadFile _
  
  d3.select("#es-export")
    .on("click", onExport _)
    
  val sourceButton = jQuery("#es-graph-mode-edit")
  val pipelineButton = jQuery("#es-graph-mode-pipeline")
  
  sourceButton.on("click", { ev: JQueryEventObject => 
    editor.swapDoc(sourceDoc)
    null
  })
  pipelineButton.on("click", { ev: JQueryEventObject => 
    editor.swapDoc(pipelineDoc)
    null
  })
  
  val pipelineStepButton = jQuery("#es-pipeline-step")
  val pipelineStepMicroButton = jQuery("#es-pipeline-step-micro")
  val pipelineRunButton = jQuery("#es-pipeline-run")
  val pipelineResetButton = jQuery("#es-pipeline-reset")
  
  pipelineStepButton.on("click", { ev: JQueryEventObject => 
    triggerAction(Pipeline.StepPipeline())
    null
  })
  pipelineStepMicroButton.on("click", { ev: JQueryEventObject => 
    triggerAction(Structure.StructureDoReplayStep())
    null
  })
  pipelineRunButton.on("click", { ev: JQueryEventObject => 
    triggerAction(Pipeline.RunPipeline())
    null
  })
  pipelineResetButton.on("click", { ev: JQueryEventObject => 
    triggerAction(Pipeline.ResetPipeline())
    null
  })
  
  def triggerLineAction(line: Int) = {
    for {
      (meta, info, l) <- runners
      if l == line
    } {
      triggerAction(new Pipeline.RunMetaRunner(meta, info, line))
    }
  }

  def onChange() {
    val newText = sourceDoc.getValue()
    if (newText != lastText) {
      lastText = newText
      triggerAction(Source.LoadDefinition(newText))
    }
    
    val newPipelineText = pipelineDoc.getValue()
    if (newPipelineText != lastPipelineText) {
      lastPipelineText = newPipelineText
      triggerAction(Pipeline.LoadPipeline(newPipelineText))
    }
  }
    
  def onExport(et: EventTarget) {
    
    val newText = sourceDoc.getValue()
    val textUri = URIUtils.encodeURIComponent(newText)
    d3.select("#es-export-text")
      .attr("href", "data:text/plain;charset=utf-8,"+textUri)
      .attr("download", name + ".txt")
      
    val svg = dom.document.getElementById("es-graph").asInstanceOf[SVGSVGElement]
    println(svg)
    val svgMutator = d3.select(svg)
    val sel = svgMutator.selectAll[dom.raw.Element]("path")(0).foreach { e: EventTarget =>
        if (js.isUndefined(e)) {
          println("undefined!!")
          "a"
        } else {
          println(e)
          val el = e.asInstanceOf[dom.raw.Element]
          el.setAttribute("stroke-dasharray",
              dom.window.getComputedStyle(el, "").strokeDasharray)
         
        } }
    val style = dom.document.getElementById("es-graph-style").innerHTML
    val svgUri = URIUtils.encodeURIComponent(
      "<svg>\n" +
      "<style type=\"text/css\">\n <![CDATA[" +
      style +
      " ]]>\n</style>" +
      svgMutator.html() +
      "\n</svg>"
    )
    d3.select("#es-export-svg")
      .attr("href", "data:text/plain;charset=utf-8,"+svgUri)
      .attr("download", name + ".svg")
    
    for (ts <- currentStructure) {
      val tsCsvUri = URIUtils.encodeURIComponent(ts.step.toCsvString())
      d3.select("#es-export-csv")
        .attr("href", "data:text/plain;charset=utf-8,"+tsCsvUri)
        .attr("download", name + ".csv")
    }
    
  }
  
  def onLoadFile(ev: Event) {
    val fileBlob = ev.target.asInstanceOf[HTMLInputElement].files(0)
    
    println("reading file: " + fileBlob.name)
    
    val reader = new dom.FileReader()
    reader.onload = (e: UIEvent) => {
      val contents = reader.result.asInstanceOf[String]
      triggerAction(Source.LoadDefinition(contents))
    }
    
    reader.readAsText(fileBlob)
  }
  
  def onImport(ev: Event) {
    val fileBlob = ev.target.asInstanceOf[HTMLInputElement].files(0)
    
    println("reading file: " + fileBlob.name)
    
    val reader = new dom.FileReader()
    reader.onload = (e: UIEvent) => {
      val contents = reader.result.asInstanceOf[String]
      triggerAction(Source.LoadDefinition(contents))
    }
    
    reader.readAsText(fileBlob)
  }
  
  def setCode(code: String) {
    if (sourceDoc.getValue() != code) {
      sourceDoc.setValue(code)
    }
  }

  def setErrors(code: String, errs: List[Source.Problem]) {
    setCode(code)
    editor.clearGutter(PROBLEM_GUTTER)
    for (Source.Problem(msg, line, col) <- errs) {
      val problemNode = dom.document.createElement("div").asInstanceOf[HTMLElement]
      problemNode.setAttribute("class", "es-problem")
      problemNode.setAttribute("title", msg)
      editor.setGutterMarker(line - 1, PROBLEM_GUTTER, problemNode)
    }
  }

  def setRunners(runners: List[(String, List[String], Int)]) = {
    this.runners = runners
  }
  
  def setSamples(samples: List[Samples.Example]) {
    val list = js.Array[Samples.Example]()
    list.appendAll(samples)
    d3.select("#es-load .dropdown-menu").selectAll(".es-load-example")
      .data(list)
      .enter()
        .append("li")
        .classed("es-load-example", true)
        .classed("divider", (s: Samples.Example, i: Int) => s.slug == "diamond")
        .html((s: Samples.Example, i: Int) => "<a href=\"#" + s.slug + "\">" + s.name + "</a>")
        .on("click", {(s: Samples.Example, i: Int) => 
          triggerAction(Source.LoadDefinition(s.code))
        })
  }
  
  def setOperations(ops: List[StructureOperation] ) {
    println("set operations: "+ops)
    val groupedOps = ops.groupBy(_.category)
    
    for (
      (group, ops2) <- groupedOps
    ) {
      val list = js.Array[(String, String, String)]()
      list.appendAll(ops2 map { a => (a.slug, a.name, a.description) })
      d3.select("#es-transform .dropdown-menu."+group).selectAll[(String, String, String)](".es-apply-analyzer")
        .data(list, (_: (String, String, String))._1)
        .enter()
          .append("li")
          .classed("es-apply-analyzer", true)
          .html((a: (String, String, String), i: Int) => "<a href=\"#" + a._1 + "\" title=\""+ a._3 + "\">" + a._2 + "</a>")
          .on("click", {(a: (String, String, String), i: Int) => 
            triggerAction(Structure.StructureCallOperation(a._1))
          })
    }
  }
  
  def setPipelineStatus(entries: List[Pipeline.LineInfo]) {
    editor.clearGutter(PROBLEM_GUTTER)
    entries foreach {
      case Pipeline.CurrentLine(line) =>
        currentPipelineLine = line
        val node = dom.document.createElement("div").asInstanceOf[HTMLElement]
        node.setAttribute("class", "es-pipeline-current")
        node.setAttribute("title", "current line")
        editor.setGutterMarker(line, PROBLEM_GUTTER, node)
      case Pipeline.OperationLine(line, explanation) =>
        currentPipelineLine = line
        val node = dom.document.createElement("div").asInstanceOf[HTMLElement]
        node.setAttribute("class", "es-pipeline-operation")
        node.setAttribute("title", explanation)
        editor.setGutterMarker(line, PROBLEM_GUTTER, node)
    }
  }
  
  def setReplay(replay: List[() => AlgorithmLogging.LogEntry[NodeID]]) {
    val node = dom.document.createElement("ul").asInstanceOf[HTMLElement]
    for ((le, i) <- replay.zipWithIndex) {
      val leChild = dom.document.createElement("li").asInstanceOf[HTMLElement]
      leChild.innerHTML = i.toString + ": " + (le() match {
        case AlgorithmLogging.LogRelation(_, comment) =>
          comment
        case AlgorithmLogging.LogRichRelation(_, comment) =>
          comment
        case AlgorithmLogging.LogSpectrum(_, _, _, _, _, comment) =>
          comment
        case _ =>
          ""
      })
      leChild.setAttribute("class", "es-pipeline-replay-step")
      jQuery(leChild).on("click", { ev: JQueryEventObject => 
        triggerAction(Structure.StructureDoReplayStep(i))
        null
      })
      node.appendChild(leChild)
    }
    pipelineReplayWidget foreach (_.clear())
    pipelineReplayWidget = Some(editor.addLineWidget(currentPipelineLine, node))
  }
  
  def setStructure(ts: Structure.TSStructure) = {
    currentStructure = Some(ts)
  }
  
  def getURLSampleSlug() = {
    dom.window.location.hash.substring(1)
  }
  
  def notify(change: ModelComponent.Change) = change match {
    case Source.SourceChange(source, ast) =>
      setCode(source)
      setRunners(ast.defs.collect {
          case MetaDeclaration(key, value, pos) => (key, value, pos.line)
      })
    case Source.ProblemChange(source, errs) =>
      setErrors(source, errs)
    case Source.ExamplesChange(samples) => 
      setSamples(samples)
    case Pipeline.PipelineStatusChange(statusEntries) =>
      setPipelineStatus(statusEntries)
    case Structure.StructureOperationsChanged(ops) =>
      setOperations(ops.values.toList)
    case Structure.StructureReplayChange(replay) =>
      setReplay(replay)
    case Structure.StructureChange(tsStructure) =>
      setStructure(tsStructure)
    case _ => 
  }
}