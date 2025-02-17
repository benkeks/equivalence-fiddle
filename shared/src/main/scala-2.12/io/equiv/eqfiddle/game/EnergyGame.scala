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
        if (possibleMoves.isEmpty || possibleMoves.exists(_.isEmpty)) {
          Nil // return empty if one defender option is un-winnable for attacker
        } else {
          var productMoves = possibleMoves.head
          for (paretoFront <- possibleMoves.tail) {
            var newMoves: List[Energy] = Nil
            for (otherMove <- paretoFront; knownMove <- productMoves) {
              val productOption = knownMove lub otherMove
              // only include those new moves that are not dominated by one that already exists.
              if (!newMoves.exists(m => m <= productOption)) {
                newMoves = productOption :: newMoves
              }
            }
            productMoves = newMoves
          }
          productMoves
        }
        /*
        // functional implementation:
        val productMoves =
          possibleMoves.reduceLeft(
            (b, a) => (b.flatMap(i => a.map(j => i lub j))))
        productMoves.toSet
        */
    }
  }

  private def filterMinimal(energies: List[Energy]) = {
    // if energy list becomes big, prune dominated energies on-the-fly at defender positions
    // to prevent exponential blowup of options.
    if (energies.size > 2) {
      energies.filterNot(e1 => energies.exists(e2 => e2 < e1))
    } else {
      energies
    }
  }
}

object EnergyGame {

  // hard code values to prevent spamming of object creation in comparisons
  private val EnergySame = Some(0)
  private val EnergyLower = Some(-1)
  private val EnergyHigher = Some(1)

  final class Energy private(val vector: Array[Int]) extends PartiallyOrdered[Energy] {

    def dim() = vector.length

    val indices = 0 until vector.length

    def apply(n: Int) = vector(n)

    override val hashCode: Int = {
      indices.fold(0){ (hash, i) => (hash << 3) ^ vector(i) }
    }

    override def toString(): String =
      vector.mkString("(", "," ,")")

    def isNonNegative() =
      vector.forall(_ >= 0)

    /* assumes that only energies of equal length are being compared */
    override def tryCompareTo[B >: Energy](that: B)(implicit evidence$1: B => PartiallyOrdered[B]): Option[Int] = {
      that match {
        case that: Energy =>
          if (indices.forall(i => this.vector(i) == that.vector(i))) { //this.vector == that.vector) {
            EnergySame
          } else {
            if (indices.forall(i => this.vector(i) >= that.vector(i))) {
              EnergyHigher
            } else if (indices.forall(i => this.vector(i) <= that.vector(i))) {
              EnergyLower
            } else {
              None
            }
          }
        case _ => None
      }
    }

    override def equals(that: Any): Boolean = that match {
      case that: Energy =>
        this.dim == that.dim && indices.forall(i => this.vector(i) == that.vector(i))
      case _ =>
        false
    }

    def lub(that: Energy): Energy = {
      val newEnergy = new Array[Int](dim)
      indices.foreach { i => newEnergy(i) = Math.max(this.vector(i), that.vector(i)) }
      Energy(newEnergy)
    }

    def glb(that: Energy): Energy = {
      Energy(Array.tabulate(vector.length)(i => Math.min(this.vector(i), that.vector(i))))
    }
  }

  object Energy {

    def apply(vector: Array[Int]): Energy = {
      new Energy(vector)
    }

    def zeroEnergy(dim: Int) = {
      new Energy(new Array[Int](dim))
    }

    def spikeEnergy(dim: Int, spikePos: Int, spikeVal: Int) = {
      val spikeArray = new Array[Int](dim)
      spikeArray(spikePos) = spikeVal
      new Energy(spikeArray)
    }

  }

  final case class EnergyUpdate(
      /** component updates
       * - non-positive Ints = relative updates
       * - positive Ints less or equal to dimensionality = min of current row with other row of number (starting to count at index 1)
       * - positive Ints above dimensionality = relative updates (after subtraction of dimensionality)
      */
      val updates: Array[Int],
      /** bound height of energy lattice */
      energyCap: Int = Int.MaxValue
    ) {

    def this(ups: Int*) = this(ups.toArray)

    private val isZero = updates.forall(_ == 0)

    def applyEnergyUpdate(e: Energy): Energy = {
      val newEnergies = for {
        (u, i) <- updates.zipWithIndex
      } yield {
        if (u <= 0) {
          e(i) + u
        } else if (u >= updates.length) {
          e(i) + u - updates.length
        } else {
          Math.min(e(i), e(u - 1))
        }
      }
      Energy(newEnergies)
    }

    def applyEnergyUpdateInfinity(e: Energy): Energy = {
      val newEnergies = for {
        (u, i) <- updates.zipWithIndex
      } yield {
        if (u <= 0) {
          if (e(i) == Int.MaxValue) e(i) else e(i) + u
        } else if (u > updates.length) {
          if (e(i) == Int.MaxValue) e(i) else e(i) + u - updates.length
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
        val newEnergies = new Array[Int](updates.length)
        for {
          i <- 0 until updates.length
        } {
          newEnergies(i) = if (updates(i) <= 0) {
            Math.min(e(i) - updates(i), energyCap)
          } else if (updates(i) > updates.length ) {
            Math.min(Math.max(e(i) - updates(i) + updates.length, 0), energyCap)
          } else {
            e(i)
          }
        }
        for {
          i <- 0 until updates.length
          if (updates(i) > 0 && updates(i) <= updates.length)
        } {
          newEnergies(updates(i) - 1) = Math.max(newEnergies(updates(i) - 1), e(i))
        }
        Energy(newEnergies)
      }
    }

    override def toString(): String = {
      updates.zipWithIndex.map {
        case (u, i) => if (u <= 0 || u > updates.length) u.toString() else s"min{$u,${i + 1}}"
      }.mkString("(", ",", ")")
    }
  }
  
  object EnergyUpdate {
    def add(z: Int, dim: Int) = {
      if (z <= 0)
        z
      else
        z + dim
    }

    def minWith(i: Int) = {
      i
    }
  }
}