package de.bbisping.eqfiddle.spectroscopy

import scala.scalajs.js
import scala.scalajs.js.annotation.{JSExportTopLevel, JSExport}

import de.bbisping.eqfiddle.algo.AlgorithmLogging
import de.bbisping.eqfiddle.ts.WeakTransitionSystem
import de.bbisping.eqfiddle.util.LabeledRelation

object EquivalenceSpectroscopeAPI {
  type WeakTS = WeakTransitionSystem[String, String, String]

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
      if (js.isUndefined(lts.silent)) "tau" else lts.silent.asInstanceOf[String]

    new WeakTransitionSystem[String, String, String](step, nodeLabeling, Set(silentAction))
  }

  @JSExportTopLevel("performSpectroscopy")
  def performSpectroscopy(lts: WeakTS, p1: String, p2: String) = {
    AlgorithmLogging.debugLogActive = false
    val algo = new PositionalSpectroscopy(lts, List(p1, p2))
    val result = algo.compute()

    import js.JSConverters._
    for {
      res <- result.relationItems.toJSArray
    } yield res.serialize(_.toJSArray, _.toJSDictionary)
  }
}