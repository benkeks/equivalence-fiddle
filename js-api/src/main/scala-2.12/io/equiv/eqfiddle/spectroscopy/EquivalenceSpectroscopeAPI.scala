package io.equiv.eqfiddle.spectroscopy

import scala.scalajs.js
import scala.scalajs.js.annotation.{JSExportTopLevel, JSExport, JSExportAll}
import scala.scalajs.js.JSConverters._

import io.equiv.eqfiddle.algo.AlgorithmLogging
import io.equiv.eqfiddle.ts.WeakTransitionSystem
import io.equiv.eqfiddle.util.LabeledRelation
import io.equiv.eqfiddle.spectroscopy.helpers.CytoscapeHelpers
import io.equiv.eqfiddle.hml.Spectrum
import io.equiv.eqfiddle.hml.ObservationNotionStrong

object EquivalenceSpectroscopeAPI {

  @JSExportAll
  class WeakTS(
      step: LabeledRelation[String, String],
      nodeLabeling: Map[String, String],
      override val silentActions: Set[String])
    extends WeakTransitionSystem[String, String, String](step, nodeLabeling, silentActions)
      with CytoscapeHelpers {

    def _toMime() = {
      Map("text/html" -> buildCytoscape(cytoNodes(), cytoEdges()) ).toJSDictionary
    }

    private def cytoNodes() = {
      val nodeStrings = for {
        (id, label) <- nodeLabeling.toIterable
      } yield s"{ data: { id: '$id', name: '$label'} }"
      nodeStrings.mkString("[", ",", "]")
    }

    private def cytoEdges() = {
      val edgeStrings = for {
        (src, label, target) <- step.tupleSet
      } yield s"{ data: { source: '$src', target: '$target', label: '$label'} }"
      edgeStrings.mkString("[", ",", "]")
    }
  }

  @JSExportTopLevel("loadLTS")
  def loadLTS(ltsObject: js.Dynamic): WeakTS = {
    val lts = ltsObject.lts
    val initialState = lts.initialState
    val stateEntries: js.Dictionary[js.Dictionary[js.Dynamic]] = lts.states.asInstanceOf[js.Dictionary[js.Dictionary[js.Dynamic]]]
    val states = stateEntries.keys

    val transitions = for {
      (s, obj) <- stateEntries
      trans <- obj("transitions").asInstanceOf[js.Array[js.Dynamic]]
    } yield
      (s.asInstanceOf[String],
      trans.label.asInstanceOf[String],
      trans.target.asInstanceOf[String])
    val step = new LabeledRelation(transitions.toSet)
  
    val nodeLabeling = (for {
      (s, obj) <- stateEntries
      proc = obj("ccs").asInstanceOf[String]
    } yield (s, proc)).toMap

    val silentAction: String =
      if (js.isUndefined(lts.silent)) "τ" else lts.silent.asInstanceOf[String]

    new WeakTS(step, nodeLabeling, Set(silentAction))
  }

  @JSExportTopLevel("performSpectroscopy")
  def performSpectroscopy(lts: WeakTS, p1: String, p2: String) = {
    AlgorithmLogging.debugLogActive = false
    val algo = new StrongSpectroscopy(lts)
    val result = algo.decideAll(List((p1, p2)))

    for {
      res <- result.relationItems.toJSArray
    } yield res.serialize(_.toJSArray, _.toJSDictionary)
  }

  @JSExportTopLevel("LTBTS")
  def LTBTS() = {
    val classes = for {
      Spectrum.EquivalenceNotion(name, obsNotion) <- ObservationNotionStrong.LTBTS.notions
    } yield (name, obsNotion.toTuple.productIterator.toIterable.toJSArray)
    classes.toMap.toJSDictionary
  }
}