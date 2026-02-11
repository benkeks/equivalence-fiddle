package org.denigma.codemirror.extensions

import org.denigma.codemirror.{EditorChangeLike, Editor, EditorConfiguration}
import scala.scalajs.js

class ExtendedChange(val change: EditorChangeLike) extends AnyVal {

  def mergeSpans(other: (Int, Int)): (Int, Int) = (changedSpan, other) match {
    case ((min1, max1), (min2, max2)) =>
      (Math.min(min1, min2), Math.max(max1, max2))
  }

  def changedSpan = (
    change.from.line,
    change.to.line - (if (change.removed.length > 1) change.removed.length - 1 else 0) + (if (change.text.length > 1) change.text.length - 1 else 0)
  )

  def newLines: Map[Int, String] =
    change.text.zipWithIndex.map {
      case (s, i) => (i + change.from.line, s)
    }.toMap
}

class ExtendedEditor(val editor: Editor) extends AnyVal {
  def addOnGutterClick(fun: (Editor, Int) => Unit) = {
    val handler: js.Function2[Editor, _, Unit] = fun
    editor.on("gutterClick", handler)
  }

  def addOnChange(fun: (Editor, EditorChangeLike) => Unit) = {
    val handler: js.Function2[Editor, _, Unit] = fun
    editor.on("change", handler)
  }

  def addOnChanges(fun: (Editor, js.Array[EditorChangeLike]) => Unit) = {
    val handler: js.Function2[Editor, _, Unit] = fun
    editor.on("changes", handler)
  }

  def addBeforeChange(fun: (Editor, EditorChangeLike) => Unit) = {
    val handler: js.Function2[Editor, _, Unit] = fun
    editor.on("change", handler)
  }

  def lineText(line: Int): String = {
    editor.lineInfo(line).text
  }

  def linesText(lines: Seq[Int]): Seq[(Int, String)] =
    lines.map(num => num -> lineText(num))
}

package object extensions {
  implicit def editorExtended(editor: Editor): ExtendedEditor = new ExtendedEditor(editor)

  implicit def editorExtended(change: EditorChangeLike): ExtendedChange = new ExtendedChange(change)

  implicit def editorConfigToConfig(builder: EditorConfigurationBuilder): EditorConfiguration = builder.result
}
