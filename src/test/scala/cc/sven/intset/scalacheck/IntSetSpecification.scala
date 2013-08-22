package cc.sven.intset.scalacheck

import org.scalacheck.Properties
import org.scalacheck.Prop.forAll
import cc.sven.intset._
import cc.sven.bounded._
import cc.sven.intset.IntSet


object IntSetSpecification extends Properties("IntSet") {
  property("bitVector identity[Int]") = forAll((a: Int) => IntSet.fromBitVector[Int](IntSet.toBitVector(a)) == a)
  property("set eq IntSet[Int]") = forAll{
    (a : Set[Int]) =>
      val b = IntSet(a)
      a.forall(b.contains(_)) && b.forall(a.contains(_))
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
//[- AW -] some operations on the copied set to show independence of the sets?
  }
  property("set cbdd.bdd ref equal") = forAll{
    (a : Set[Int]) =>
      val as = a.toList
      val bs = util.Random.shuffle(as)
      val cs = util.Random.shuffle(as)
      //Build sets randomly - just to be sure
      val b = (IntSet[Int]() /: bs)(_ + _)
      val c = (IntSet[Int]() /: cs)(_ + _)
      b.cbdd.bdd eq c.cbdd.bdd
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
      val boundedBits = BoundedBits.IntIsBoundedBits
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
  property("ival set") = forAll{
    (a : Int, b : Int) =>
      val a_ = a % 1000
      val b_ = b % 1000
      val lo = a_ min b_
      val hi = a_ max b_
      val ival = Ival(lo, hi)
      val refSet = ival.toSet
      val bddSet = IntSet(ival)
      bddSet.forall(refSet.contains(_)) && refSet.forall(bddSet.contains(_))
  }
  property("intersect") = forAll{
    (a : Set[Int], b : Set[Int]) =>
      val refSet = a & b
      val bddSet = IntSet(a) & IntSet(b)
      bddSet.forall(refSet.contains(_)) && refSet.forall(bddSet.contains(_))
  }
  property("union") = forAll{
    (a : Set[Int], b : Set[Int]) =>
      val refSet = a | b
      val bddSet = IntSet(a) | IntSet(b)
      bddSet.forall(refSet.contains(_)) && refSet.forall(bddSet.contains(_))
  }
/* [- AW -]
   Wichtigere Funktionalitaeten:
   teilmenge
   isFull
   isEmpty
   iterator 
   liste von elementen
   bitextract: first:last bits ausschneiden
   set mul set
   set plus set
   signextend
   zerofill
   Zur info: Jakstab RTL operators:
	UNKNOWN,
	
	// Operators for changing bitwidth
	CAST, 
	SIGN_EXTEND("sign_extend"),
	ZERO_FILL("zero_fill"),
	FSIZE,

	// Comparison
	EQUAL("=="), 
	LESS("<"), // Signed
	LESS_OR_EQUAL("<="), // Signed
	UNSIGNED_LESS("u<"), 
	UNSIGNED_LESS_OR_EQUAL("u<="),

	// Unary operators
	NOT("!"),
	NEG("-"),
	
	// Associative commutative bitwise arithmetic operators
	AND("&"), 
	OR("|"), 
	XOR("^"),
	PLUS("+"),
	MUL("*"),
	FMUL,
	FDIV,

	// Other bitwise arithmetic operators
	DIV, 
	MOD, 
	POWER_OF,

	// Bitwise shift operations
	SHR(">>>"), 
	SAR(">>"), / * Shift right with sign extension * /
	SHL("<<"), 
	ROL, 
	ROR, 
	ROLC, 
	RORC / * Rotate with carry * /
	;
*/
}