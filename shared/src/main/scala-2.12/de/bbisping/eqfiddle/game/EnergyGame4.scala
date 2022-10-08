package de.bbisping.eqfiddle.game

trait EnergyGame4 extends SimpleGame with GameLazyDecision[EnergyGame4.Energy] {

  import EnergyGame4._

  def weight(gn1: GameNode, gn2: GameNode): EnergyUpdate

  override def priceIsBetter(p1: Energy, p2: Energy): Boolean = p1 < p2

  override def priceIsBetterOrEq(p1: Energy, p2: Energy): Boolean = p1 <= p2

  override def computeCurrentPrice(node: GameNode): Iterable[Energy] = {
    node match {
      case an: AttackerNode =>
        for {
          s <- successors(node)
          sE <- attackerVictoryPrices(s)
        } yield weight(node, s).unapplyEnergyUpdate(sE)
      case dn: DefenderNode =>
        val possibleMoves = for {
          s <- successors(node)
          w = weight(node, s)
        } yield attackerVictoryPrices(s).map(w.unapplyEnergyUpdate(_))
        val productMoves =
          possibleMoves.reduceLeft(
            (b, a) => b.flatMap(i => a.map(j => i lub j)))
        productMoves.toSet
    }
  }
}

object EnergyGame4 {

  // hard code values to prevent spamming of object creation in comparisons
  private val EnergySame = Some(0)
  private val EnergyLower = Some(-1)
  private val EnergyHigher = Some(1)

  final class Energy(val u: Int, val v: Int, val w: Int, val x: Int)  extends PartiallyOrdered[Energy] {

    def dim() = 4

    def apply(n: Int) = if (n == 0) u else if (n == 1) v else if (n == 2) w else x

    override def tryCompareTo[B >: Energy](that: B)(implicit evidence$1: B => PartiallyOrdered[B]): Option[Int] = {
      that match {
        case that: Energy =>
          if (this.u == that.u && this.v == that.v && this.w == that.w && this.x == that.x) {
            EnergySame
          } else {
            if (this.u >= that.u && this.v >= that.v && this.w >= that.w && this.x >= that.x) {
              EnergyHigher
            } else if (this.u <= that.u && this.v <= that.v && this.w <= that.w && this.x <= that.x) {
              EnergyLower
            } else {
              None
            }
          }
        case _ => None
      }
    }

    def lub(that: Energy): Energy = {
      new Energy(Math.max(this.u, that.u), Math.max(this.v, that.v), Math.max(this.w, that.w), Math.max(this.x, that.x))
    }

    def glb(that: Energy): Energy = {
      new Energy(Math.min(this.u, that.u), Math.min(this.v, that.v), Math.min(this.w, that.w), Math.min(this.x, that.x))
    }

    override def toString(): String = {
      s"($u,$v,$w,$x)"
    }
  }

  def zeroEnergy(dim: Int) = {
    new Energy(0,0,0,0)
  }

  def spikeEnergy(dim: Int, spikePos: Int, spikeVal: Int) = {
    spikePos match {
      case 0 => new Energy(spikeVal, 0, 0, 0)
      case 1 => new Energy(0, spikeVal, 0, 0)
      case 2 => new Energy(0, 0, spikeVal, 0)
      case _ => new Energy(0, 0, 0, spikeVal)
    }
  }

  final case class EnergyUpdate(
      /** component updates
       * - non-positive Ints = relative updates
       * - positive Ints = min of current row with other row of number (starting to count at index 1)
      */
      updateU: Int,
      updateV: Int,
      updateW: Int,
      updateX: Int,
      /** bound height of energy lattice */
      energyCap: Int = Int.MaxValue
    ) {

    //def this(ups: Int*) = this(ups.toIndexedSeq)

    private val isZero = updateU == 0 && updateV == 0 && updateW == 0 && updateX == 0

    def applyEnergyUpdate(e: Energy): Energy = {
      new Energy(
        if (updateU <= 0) e.u + updateU else Math.min(e.u, e(updateU - 1)),
        if (updateV <= 0) e.v + updateV else Math.min(e.v, e(updateV - 1)),
        if (updateW <= 0) e.w + updateW else Math.min(e.w, e(updateW - 1)),
        if (updateX <= 0) e.x + updateX else Math.min(e.x, e(updateX - 1))
      )
    }

    def unapplyEnergyUpdate(e: Energy): Energy = {
      if (isZero) {
        e
      } else {
        var newU = if (updateU <= 0) Math.min(e.u - updateU, energyCap) else e.u
        var newV = if (updateV <= 0) Math.min(e.v - updateV, energyCap) else e.v
        var newW = if (updateW <= 0) Math.min(e.w - updateW, energyCap) else e.w
        var newX = if (updateX <= 0) Math.min(e.x - updateX, energyCap) else e.x

        if (updateU > 0) {
          newU = Math.max(newU, e(updateU-1))
        }
        if (updateV > 0) {
          newV = Math.max(newV, e(updateV-1))
        }
        if (updateW > 0) {
          newW = Math.max(newW, e(updateW-1))
        }
        if (updateX > 0) {
          newX = Math.max(newX, e(updateX-1))
        }
        // if (updateV > 0) {
        //   if (updateV == 1) newU = Math.max(newU, e.v)
        //   else if (updateV == 2) newV = Math.max(newV, e.v)
        //   else if (updateV == 3) newW = Math.max(newW, e.v)
        //   else if (updateV == 4) newX = Math.max(newX, e.v)
        // }
        // if (updateW > 0) {
        //   if (updateW == 1) newU = Math.max(newU, e.w)
        //   else if (updateW == 2) newV = Math.max(newV, e.w)
        //   else if (updateW == 3) newW = Math.max(newW, e.w)
        //   else if (updateW == 4) newX = Math.max(newX, e.w)
        // }
        // if (updateX > 0) {
        //   if (updateX == 1) newU = Math.max(newU, e.x)
        //   else if (updateX == 2) newV = Math.max(newV, e.x)
        //   else if (updateX == 3) newW = Math.max(newW, e.x)
        //   else if (updateX == 4) newX = Math.max(newX, e.x)
        // }
        new Energy(newU, newV, newW, newX)
      }
    }

    override def toString(): String = {
      s"($updateU,$updateW,$updateW,$updateX)"
    }
  }
  
}