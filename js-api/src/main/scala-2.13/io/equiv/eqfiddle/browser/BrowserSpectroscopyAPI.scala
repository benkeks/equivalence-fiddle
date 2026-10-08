package io.equiv.eqfiddle.browser

import scala.scalajs.js
import scala.scalajs.js.annotation.{JSExport, JSExportTopLevel}
import scala.scalajs.js.JSConverters._
import scala.util.control.NonFatal
import io.equiv.eqfiddle.spectroscopy.{CCSSpectroscopy, Spectroscopy}
import io.equiv.eqfiddle.tool.model.NodeID

@JSExportTopLevel("equiv")
object BrowserSpectroscopyAPI {
  private var latest: js.UndefOr[js.Object] = js.undefined

  @JSExport
  def lastResult: js.UndefOr[js.Object] = latest

  @JSExport
  def capabilities(): js.Object = js.Dynamic.literal(
    apiVersion = 1,
    execution = "synchronous",
    semantics = js.Array("strong", "weak"),
    defaultMaxStates = 5000,
    operations = js.Array("spectroscopy", "check"),
    requestFields = js.Array("source", "left", "right", "semantics", "maxStates", "operation", "notion"),
    checkNotions = Spectroscopy.checkNotions.toList.sorted.toJSArray,
    relationDirection = "leftToRight means the left process's observations are included in the right's; for simulation, the right simulates the left.",
    fragmentPrefix = "#request=",
    documentation = "https://github.com/benkeks/equivalence-fiddle/blob/main/doc/browser-api.md")

  private def failure(code: String, message: String, line: Int = 0, column: Int = 0): js.Dynamic =
    js.Dynamic.literal(apiVersion = 1, status = "error",
      error = js.Dynamic.literal(code = code, message = message, line = line, column = column))

  private def remember(result: js.Any): js.Object = {
    val response = result.asInstanceOf[js.Object]
    latest = response
    response
  }

  private def decode(value: js.Any, checking: Boolean): Either[String, CCSSpectroscopy.Request] = {
    if (value == null || js.typeOf(value) != "object" || js.Array.isArray(value)) {
      return Left("Expected a request object.")
    }
    val fields = value.asInstanceOf[js.Dictionary[js.Any]]
    def string(name: String): Option[String] = fields.get(name)
      .filter(value => js.typeOf(value) == "string").map(_.asInstanceOf[String])
    val source = string("source")
    val left = string("left").filter(_.nonEmpty)
    val right = string("right").filter(_.nonEmpty)
    if (source.isEmpty || left.isEmpty || right.isEmpty) {
      return Left("source, left and right must be strings; process names must not be empty.")
    }
    val semantics = if (checking || !fields.contains("semantics")) Some("strong") else string("semantics")
    if (!semantics.exists(value => value == "strong" || value == "weak")) {
      return Left("semantics must be strong or weak.")
    }
    val maxStates = fields.get("maxStates") match {
      case None => 5000
      case Some(value) if js.typeOf(value) == "number" =>
        val number = value.asInstanceOf[Double]
        if (number < 1 || number > Int.MaxValue || number != number.toInt.toDouble) {
          return Left("maxStates must be a positive integer no greater than 2147483647.")
        }
        number.toInt
      case _ => return Left("maxStates must be a positive integer.")
    }
    Right(CCSSpectroscopy.Request(source.get, left.get, right.get, semantics.contains("weak"), maxStates))
  }

  @JSExport
  def spectroscopy(request: js.Any): js.Object = execute(request, checking = false)

  @JSExport
  def check(request: js.Any): js.Object = execute(request, checking = true)

  private def execute(request: js.Any, checking: Boolean): js.Object = {
    val response = try {
      decode(request, checking) match {
        case Left(message) => failure("INVALID_REQUEST", message)
        case Right(input) =>
          val notion = request.asInstanceOf[js.Dictionary[js.Any]].get("notion")
            .filter(value => js.typeOf(value) == "string").map(_.asInstanceOf[String]).filter(_.nonEmpty)
          if (checking && notion.isEmpty) return remember(failure("INVALID_REQUEST", "check requires a notion name."))
          val normalized = js.Dynamic.literal(source = input.source, left = input.left,
            right = input.right, semantics = (if (input.weak) "weak" else "strong"), maxStates = input.maxStates)
          val output = if (checking) {
            normalized.updateDynamic("operation")("check")
            normalized.updateDynamic("notion")(notion.get)
            normalized.updateDynamic("semantics")(if (Spectroscopy.isWeakNotion(notion.get)) "weak" else "strong")
            CCSSpectroscopy.check(input, notion.get) match {
              case Left(error) => failure(error.code, error.message, error.line, error.column)
              case Right(analysis) =>
                val items = analysis.result.items
                val leftToRight = items.find(item => item.left == NodeID(input.left) && item.right == NodeID(input.right)).get.isMaintained
                val rightToLeft = items.find(item => item.left == NodeID(input.right) && item.right == NodeID(input.left)).get.isMaintained
                val relation = analysis.result.relation.toList.map {
                  case (left, action, right) => js.Array(left.toString, action, right.toString)
                }.toJSArray
                js.Dynamic.literal(apiVersion = 1, status = "complete", notion = notion.get,
                  leftToRight = leftToRight, rightToLeft = rightToLeft,
                  equivalent = (leftToRight && rightToLeft), relation = relation,
                  states = analysis.transitionSystem.nodes.size)
            }
          } else CCSSpectroscopy.run(input) match {
            case Left(error) => failure(error.code, error.message, error.line, error.column)
            case Right(analysis) =>
              val result = analysis.result
              val left = NodeID(input.left)
              val right = NodeID(input.right)
              val leftPreorders = result.foundImpliedPreorders(result.spectrum.notions, left, right).map(_.name).toSet
              val rightPreorders = result.foundImpliedPreorders(result.spectrum.notions, right, left).map(_.name).toSet
              val relations = result.spectrum.notions.map { notion =>
                js.Dynamic.literal(name = notion.name,
                  leftToRight = leftPreorders(notion.name), rightToLeft = rightPreorders(notion.name),
                  equivalent = (leftPreorders(notion.name) && rightPreorders(notion.name)))
              }.toJSArray
              val pairs = (result.resultFor(left, right) ++
                (if (left == right) Nil else result.resultFor(right, left)))
                .map(_.serialize(_.toJSArray, _.toJSDictionary).asInstanceOf[js.Object]).toJSArray
              js.Dynamic.literal(apiVersion = 1, status = "complete", relations = relations, pairs = pairs,
                strongestEquivalences = result.findEqs(left, right).map(_.name).toJSArray,
                states = analysis.transitionSystem.nodes.size)
          }
          output.updateDynamic("request")(normalized)
          output
      }
    } catch {
      case NonFatal(error) => failure("COMPUTATION_ERROR", Option(error.getMessage).getOrElse(error.toString))
    }
    remember(response)
  }

  @JSExport
  def spectroscopyJSON(request: String): String = {
    val parsed = try {
      Right(js.JSON.parse(request))
    } catch {
      case NonFatal(_) => Left(failure("INVALID_JSON", "Request is not valid JSON."))
    }
    val result = parsed match {
      case Right(value) =>
        val operation = if (value != null && js.typeOf(value) == "object")
          value.asInstanceOf[js.Dictionary[js.Any]].get("operation")
            .map(name => if (js.typeOf(name) == "string") name.asInstanceOf[String] else "")
          else None
        operation match {
          case None => spectroscopy(value)
          case Some("spectroscopy") => spectroscopy(value)
          case Some("check") => check(value)
          case _ => remember(failure("INVALID_REQUEST", "operation must be spectroscopy or check."))
        }
      case Left(error) => remember(error)
    }
    js.JSON.stringify(result)
  }

  @JSExport
  def requestFragment(request: js.Any): String =
    "#request=" + js.URIUtils.encodeURIComponent(js.JSON.stringify(request))

  @JSExport
  def runFragment(fragment: String): js.UndefOr[js.Object] = {
    val payload = if (fragment.startsWith("#request=")) Some(fragment.substring(9))
      else if (fragment.startsWith("#/request/")) Some(fragment.substring(10))
      else None
    payload.map { encoded =>
      try {
        spectroscopyJSON(js.URIUtils.decodeURIComponent(encoded))
        latest.get
      } catch {
        case NonFatal(_) => remember(failure("INVALID_ENCODING", "Request fragment is not valid URI-encoded JSON."))
      }
    }.orUndefined
  }
}