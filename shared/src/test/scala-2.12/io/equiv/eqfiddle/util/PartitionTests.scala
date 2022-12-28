package io.equiv.eqfiddle.util

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should

class PartitionTests extends AnyFunSpec with should.Matchers  {
  
  describe("Listing Partitions") {
    it("should yield the trivial partition for one element") {
      Partition.partitioningListsOfSet(Set(1)) should contain theSameElementsAs List(List(Set(1)))
    }
    it("should yield a binary and a trivial partiion for two elements") {
      Partition.partitioningListsOfSet(Set(1,2)).map(_.toSet) should contain theSameElementsAs List(Set(Set(1),Set(2)), Set(Set(1,2)))
    }
    it("should yield five permutations for three elements") {
      Partition.partitioningListsOfSet(Set(1,2,3)).map(_.toSet) should contain theSameElementsAs List(
        Set(Set(1),Set(2),Set(3)),
        Set(Set(1,2),Set(3)), Set(Set(1,3),Set(2)), Set(Set(3,2),Set(1)),
        Set(Set(1,2,3)))
    }
  }
}