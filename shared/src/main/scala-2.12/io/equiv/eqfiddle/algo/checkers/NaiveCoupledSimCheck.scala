package io.equiv.eqfiddle.algo.checkers

import io.equiv.eqfiddle.util.Relation
import io.equiv.eqfiddle.ts.WeakTransitionSystem

/** checks, whether a relation is a coupled simulation w.r.t some transition system and throws counter examples.*/
class NaiveCoupledSimCheck[S, A, L] (
    ts: WeakTransitionSystem[S, A, L],
    rel: Relation[S]) {
  
  def check() = {
    (checkSimulation() map ((_, "violates simulation"))) ++
      (checkCoupling() map ((_, "violates coupling")))
  }
  
  def checkSimulation() = {
    rel.tupleSet filterNot {
      case (p, q) =>
        ts.post(p) forall {
          case (a, p1s) =>
            p1s forall { p1 =>
              ts.weakPost(q, a) exists {
                q1 => rel(p1, q1)
              }
            }
        }
    }
  }
  
  def checkCoupling() = {
    rel.tupleSet filterNot {
      case (p, q) =>
        ts.silentReachable(q) exists { q1 =>
          rel(q1, p)
        }
    }
  }
  
}