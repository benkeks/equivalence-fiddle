package org.denigma.codemirror.extensions

import scala.scalajs.js

/**
 * Simple JSOptionBuilder support for building JS objects with flexible options
 */
trait OptMap extends js.Object

object OptMap {
  val noOpts: OptMap = (new js.Object).asInstanceOf[OptMap]
}

abstract class JSOptionBuilder[T <: js.Any, B <: JSOptionBuilder[T, B]](makeBuilder: OptMap => B) extends js.Object {
  val dict: OptMap

  protected def jsOpt(name: String, value: js.Any): B = {
    val newDict = dict.asInstanceOf[js.Dynamic]
    newDict.updateDynamic(name)(value)
    makeBuilder(newDict.asInstanceOf[OptMap])
  }

  def result: T = dict.asInstanceOf[T]
}
