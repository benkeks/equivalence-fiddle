package io.equiv.eqfiddle.game

trait EnergyGame extends SimpleGame with GameLazyDecision[EnergyGame.Energy] {

  import EnergyGame._

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

object EnergyGame {

  // hard code values to prevent spamming of object creation in comparisons
  private val EnergySame = Some(0)
  private val EnergyLower = Some(-1)
  private val EnergyHigher = Some(1)

  final class Energy private(val vector: IndexedSeq[Int]) extends PartiallyOrdered[Energy] {

    def dim() = vector.length

    def apply(n: Int) = vector(n)


    /* assumes that only energies of equal length are being compared */
    override def tryCompareTo[B >: Energy](that: B)(implicit evidence$1: B => PartiallyOrdered[B]): Option[Int] = {
      that match {
        case that: Energy =>
          if (this.vector == that.vector) {
            EnergySame
          } else {
            if (vector.indices.forall(i => this.vector(i) >= that.vector(i))) {
              EnergyHigher
            } else if (vector.indices.forall(i => this.vector(i) <= that.vector(i))) {
              EnergyLower
            } else {
              None
            }
          }
        case _ => None
      }
    }

    def lub(that: Energy): Energy = {
      if (this.dim() == 4) {
        Energy(Math.max(this(0), that(0)), Math.max(this(1), that(1)), Math.max(this(2), that(2)), Math.max(this(3), that(3)))
      } else {
        Energy(IndexedSeq.tabulate(vector.length)(i => Math.max(this.vector(i), that.vector(i))))
      }
    }

    def glb(that: Energy): Energy = {
      Energy(IndexedSeq.tabulate(vector.length)(i => Math.min(this.vector(i), that.vector(i))))
    }
  }

  object Energy {
    val EnergyCeiling = 3
    val EnergyDims = 4
    val EnergyCache = Array.ofDim[Energy](EnergyCeiling + 1, EnergyCeiling + 1, EnergyCeiling + 1, EnergyCeiling + 1)
    for {
      u <- 0 to EnergyCeiling
      v <- 0 to EnergyCeiling
      w <- 0 to EnergyCeiling
      x <- 0 to EnergyCeiling
    } {
      EnergyCache(u)(v)(w)(x) = new Energy(IndexedSeq(u,v,w,x))
    }

    def apply(u: Int, v: Int, w: Int, x: Int): Energy = {
      if (u <= EnergyCeiling && v <= EnergyCeiling && w <= EnergyCeiling && x <= EnergyCeiling) {
        EnergyCache(u)(v)(w)(x)
      } else {
        new Energy(IndexedSeq(u,v,w,x))
      }
    }

    def apply(vector: IndexedSeq[Int]): Energy = {
      if (vector.size == 4) {
        apply(vector(0), vector(1), vector(2), vector(3))
      } else {
        new Energy(vector)
      }
    }

    def zeroEnergy(dim: Int) = {
      if (dim == 4) Energy(0,0,0,0) else new Energy(IndexedSeq.fill[Int](dim)(0))
    }

    def spikeEnergy(dim: Int, spikePos: Int, spikeVal: Int) = {
      if (dim == 4) {
        spikePos match {
          case 0 => Energy(spikeVal, 0,0,0)
          case 1 => Energy(0, spikeVal,0,0)
          case 2 => Energy(0,0, spikeVal,0)
          case 3 => Energy(0,0,0, spikeVal)
        }
      } else {
        new Energy(IndexedSeq.tabulate(dim)(i => if (i == spikePos) spikeVal else 0))
      }
    }

  }

  final case class EnergyUpdate(
      /** component updates
       * - non-positive Ints = relative updates
       * - positive Ints = min of current row with other row of number (starting to count at index 1)
      */
      val updates: IndexedSeq[Int],
      /** bound height of energy lattice */
      energyCap: Int = Int.MaxValue
    ) {

    def this(ups: Int*) = this(ups.toIndexedSeq)

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
            Math.min(e(i) - u, energyCap)
          } else {
            e(i)
          }
        }
        val minSources = for {
          (u, i) <- updates.zipWithIndex
          if (u > 0)
        } yield {
          Energy.spikeEnergy(e.dim, u - 1, e(i))
        }
        minSources.fold(Energy(newRelativeEnergies))(_ lub _)
      }
    }

    override def toString(): String = {
      updates.zipWithIndex.map {
        case (u, i) => if (u <= 0) u.toString() else s"min{$u,${i + 1}}"
      }.mkString("(", ",", ")")
    }
  }
  
}