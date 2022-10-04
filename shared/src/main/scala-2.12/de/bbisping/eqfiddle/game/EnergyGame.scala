package de.bbisping.eqfiddle.game

trait EnergyGame extends SimpleGame with GameLazyDecision[EnergyGame.Energy] {

  import EnergyGame._

  def weight(gn1: GameNode, gn2: GameNode): EnergyUpdate

  override def priceIsBetter(p1: Energy, p2: Energy): Boolean = p1 < p2

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

object EnergyGame {

  case class Energy(val vector: List[Int]) extends PartiallyOrdered[Energy] {

    def dim() = vector.length

    def apply(n: Int) = vector(n)

    /* assumes that only energies of equal length are being compared */
    override def tryCompareTo[B >: Energy](that: B)(implicit evidence$1: B => PartiallyOrdered[B]): Option[Int] = {
      that match {
        case that: Energy =>
          if (this == that) {
            Some(0)
          } else {
            val compares = (this.vector zip that.vector)
            if (compares.forall { case (l, r) => l >= r } ) {
              Some(1)
            } else if (compares.forall { case (l, r) => l <= r } ) {
              Some(-1)
            } else {
              None
            }
          }
        case _ => None
      }
    }

    def lub(that: Energy): Energy = {
      Energy((this.vector zip that.vector).map(pair => Math.max(pair._1, pair._2)))
    }

    def glb(that: Energy): Energy = {
      Energy((this.vector zip that.vector).map(pair => Math.min(pair._1, pair._2)))
    }
  }

  def zeroEnergy(dim: Int) = {
    Energy(List.fill[Int](dim)(0))
  }

  def spikeEnergy(dim: Int, spikePos: Int, spikeVal: Int) = {
    Energy(List.tabulate(dim)(i => if (i == spikePos) spikeVal else 0))
  }

  case class EnergyUpdate(val updates: List[Int]) {
    /* non-positive Ints = relative updates; positive Ints = min of current row with other row of number... */

    private val isZero = updates.forall(_ == 0)

    def applyEnergyUpdate(e: Energy): Energy = {
      val newEnergies = for {
        (u, i) <- updates.zipWithIndex
      } yield {
        if (u <= 0) {
          e(i) + u
        } else {
          Math.min(e(i), e(u - 1))
        }
      }
      Energy(newEnergies)
    }

    def unapplyEnergyUpdate(e: Energy): Energy = {
      if (isZero) {
        e
      } else {
        val newRelativeEnergies = for {
          (u, i) <- updates.zipWithIndex
        } yield {
          if (u <= 0) {
            e(i) - u
          } else {
            e(i)
          }
        }
        val minSources = for {
          (u, i) <- updates.zipWithIndex
          if (u > 0)
        } yield {
          spikeEnergy(e.dim, i, e(u - 1))
        }
        minSources.fold(Energy(newRelativeEnergies))(_ lub _)
      }
    }
  }
  
}