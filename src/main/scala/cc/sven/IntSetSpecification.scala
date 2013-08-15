package cc.sven.intset.test

import org.scalacheck.Properties
import org.scalacheck.Prop.forAll
import cc.sven.intset._
import cc.sven.bounded._


object IntSetSpecification extends Properties("IntSet") {
  property("bitVector identity[Int]") = forAll((a: Int) => IntSet.fromBitVector[Int](IntSet.toBitVector(a)) == a)
  property("set eq IntSet[Int]") = forAll{
    (a : Set[Int]) =>
      val b = IntSet(a)
      a.forall((i : Int) => b.contains(i)) && b.forall((i : Int) => a.contains(i))
  }
  property("set cardinality") = forAll{
    (a : Set[Int]) =>
      val b = IntSet(a)
      a.size == b.size
  }
  property("set added is included") = forAll{
    (a : Set[Int], b : Int) =>
      val c = IntSet(a)
      (c + b).contains(b)
  }
  property("set subtracted not included") = forAll{
    (a : Set[Int], b : Int) =>
      val c = IntSet(a)
      !(c - b).contains(b)
  }
  property("set equal") = forAll{
    (a : Set[Int]) =>
      val b = IntSet(a)
      val c = IntSet(a)
      (b == c)
  }
  property("set size equal") = forAll{
    (a : Set[Int]) =>
      a.size == IntSet(a).sizeBigInt.intValue
  }
  property("invert twice") = forAll{
    (a : Set[Int]) =>
      (!(!IntSet(a))).seq == a
  }
  property("all ints") = forAll{
    import scala.math.BigInt._
    (a : Set[Int]) =>
      val b = IntSet(a)
      val boundedBits = BoundedBits.IntIsBoundedBit
      (b.sizeBigInt + (!b).sizeBigInt) == 2.pow(boundedBits.bits)
  }
  property("min") = forAll{
    (a : Set[Int], b : Int) =>
      val c = a + b
      IntSet(c).min == c.min
  }
  property("max") = forAll{
    (a : Set[Int], b : Int) =>
      val c = a + b
      IntSet(c).max == c.max
  }
}