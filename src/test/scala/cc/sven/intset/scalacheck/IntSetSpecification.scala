package cc.sven.intset.scalacheck

import org.scalacheck.Properties
import org.scalacheck.Prop.forAll
import cc.sven.intset._
import cc.sven.bounded._
import cc.sven.integral._
import cc.sven.intset.IntSet
import scala.sys.BooleanProp
import cc.sven.misc.Misc._
import cc.sven.tlike._

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
  property("set hashCode") = forAll{
    (a : Set[Int]) =>
      val as = a.toList
      val bs = util.Random.shuffle(as)
      val cs = util.Random.shuffle(as)
      //Build sets randomly - just to be sure
      val b = (IntSet[Int]() /: bs)(_ + _)
      val c = (IntSet[Int]() /: cs)(_ + _)
      b.hashCode == c.hashCode
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
      val boundedBits = BoundedBits.intIsBoundedBits
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
  /*property("ival set") = forAll{
    (a : Int, b : Int) =>
      val a_ = a % 1000
      val b_ = b % 1000
      val lo = a_ min b_
      val hi = a_ max b_
      val ival = Ival(lo, hi)
      val refSet = ival.toSet
      val bddSet = IntSet(ival)
      bddSet.forall(refSet.contains(_)) && refSet.forall(bddSet.contains(_))
  }*/
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
  property("subsetOf") = forAll{
    (a : Set[Int], b : Set[Int]) =>
      val a_ = IntSet(a)
      val b_ = IntSet(b)
      if(a subsetOf b) a_ subsetOf b_ else true
  }
  property("subsetOf intersection") = forAll{
    (a : Set[Int], b : Set[Int]) =>
      val c = a -- b
      val a_ = IntSet(a)
      val b_ = IntSet(b)
      val c_ = IntSet(c)
      c_ subsetOf a_
  }
  property("isEmpty") = forAll{
    (a : Set[Int]) =>
      IntSet(a).isEmpty == a.isEmpty
  }
  property("toList") = forAll{
    (a : Set[Int]) =>
      val ref = (IntSet[Int]() /: IntSet(a).toList)(_ + _)
      ref.forall(a.contains(_)) && a.forall(ref.contains(_))
  }
  property("inverse is Full") = (!IntSet[Int]()).isFull
  property("plus singleton sets") = forAll{
    (a : Int, b : Int) => IntSet(Set(a + b)) == (IntSet(a) plus IntSet(b))
  }
  property("plus sets") = forAll{
    (a : Set[Int], b : Set[Int]) =>
      val aa = IntSet(a)
      val bb = IntSet(b)
      val ref = cartesianProduct(aa, bb).map((x) => x._1 + x._2)
      val res = aa plus bb
      ref.forall(res.contains(_)) && res.forall(ref.contains(_))
  }
  property("plus is commutative") = forAll{
    (a : Set[Int], b : Set[Int]) =>
      val aa = IntSet(a)
      val bb = IntSet(b)
      (aa plus bb) == (bb plus aa)
  }
  property("random element is included") = forAll{
    (a : Set[Int], b : Int) =>
      val ab = IntSet(a + b)
      val random = ab.randomElement()
      ab.contains(random)
  }
  property("bitwise and 0") = forAll{
    (a : Set[Int], b : Int) =>
      val a_ = IntSet(a + b)
      (a_ bAnd IntSet(0)) == IntSet(0) && (IntSet(0) bAnd a_) == IntSet(0)
  }
  property("bitwise and is commutative") = forAll{
    (a : Set[Int], b : Set[Int]) =>
      val aa = IntSet(a)
      val bb = IntSet(b)
      (aa bAnd bb) == (bb bAnd aa)
  }
  property("bitwise and") = forAll{
    (a : Set[Int], b : Set[Int]) =>
      val aa = IntSet(a)
      val bb = IntSet(b)
      val ref = cartesianProduct(a, b).map((x) => x._1 & x._2)
      ref == (aa bAnd bb)
  }
  property("bitwise or ser") = forAll{
    (a : Set[Int], b : Int) =>
      val a_ = IntSet(a + b)
      (a_ bOr IntSet(-1)) == IntSet(-1) && (IntSet(-1) bOr a_) == IntSet(-1)
  }
  property("bitwise or is commutative") = forAll{
    (a : Set[Int], b : Set[Int]) =>
      val aa = IntSet(a)
      val bb = IntSet(b)
      (aa bOr bb) == (bb bOr aa)
  }
  property("bitwise or") = forAll{
    (a : Set[Int], b : Set[Int]) =>
      val aa = IntSet(a)
      val bb = IntSet(b)
      val ref = cartesianProduct(a, b).map((x) => x._1 | x._2)
      ref == (aa bOr bb)
  }
  property("bitwise not") = forAll{
    (a : Set[Int]) =>
      val aa = IntSet(a)
      val ref = a.map(~_)
      ref == aa.bNot
  }
  property("bitwise not negate") = forAll{
    (a : Set[Int]) =>
      val aa = IntSet(a)
      val ref = a.map(-_)
      ref == aa.bNot.plus(IntSet(1))
  }
  property("bit Extract IntLikeSet") = forAll{
    (a_ : Set[Int], b : Int, c : Int) =>
      val a = a_.map(_.abs)
      val aa = IntLikeSet[Int, Int](a)
      val intBits = implicitly[BoundedBits[Int]].bits
      val b_ = if(b == Int.MinValue) 1 else b.abs % intBits
      val c_ = if(c == Int.MinValue) 1 else c.abs % intBits
      val lo = b_ min c_
      val hi = b_ max c_
      val mask = (0 /: (lo to hi).toList)((acc, i) => acc | (1 << i))
      val ref = a.map((i) => ((i  & mask) >>> lo))
      val us = new IntLikeSet[Int, Int](32, aa.bitExtract(hi, lo).set)
      us == ref
  }
  property("plus IntLike") = forAll{longBittedOp(((_, x, y) => x + y), _ plus _)}
  property("and IntLike") = forAll{longBittedOp(((_, x, y) => x & y), _ bAnd _)}
  property("or IntLike") = forAll{longBittedOp(((_, x, y) => x | y), _ bOr _)}
  property("negate IntLike") = forAll{(a : Set[Long], b : Int) => longBittedOp((_, x, _) => -x, (x, _) => x.negate)(a, Set(1l), b)}
  property("bNot IntLike") = forAll{(a : Set[Long], b : Int) => longBittedOp((_, x, _) => ~x, (x, _) => x.bNot)(a, Set(1l), b)}
  property("bShr IntLike") = forAll{
    (a : Set[Long], bits : Int, toShift : Long) =>
      longBittedOp((bits_, x, s) => NBitLong.signContract(bits_, x) >>> s, (x, s) => x.bShr(s.randomElement().getValue.intValue))(a, Set((toShift % implicitly[BoundedBits[Long]].bits).abs), bits)
  }
  property("bShl IntLike") = forAll{
    (a : Set[Long], bits : Int, toShift : Long) =>
      longBittedOp((bits_, x, s) => NBitLong.signContract(bits_, x) << s, (x, s) => x.bShl(s.randomElement().getValue.intValue))(a, Set((toShift % implicitly[BoundedBits[Long]].bits).abs), bits)
  }
  property("size IntLike") = forAll{
    (a : Set[Long], bits : Int) =>
      val bits_ = NBitLong.boundBits(bits)
      val a_ = a.map(NBitLong.bound(_, bits_))
      val b = IntLikeSet[Long, NBitLong](bits_, a_.map(NBitLong(bits_, _)))
      a_.size == b.size
  }
  property("sizeGreaterThan IntLike") = forAll{
    (a : Set[Long], bits : Int, c : Int) =>
      val bits_ = NBitLong.boundBits(bits)
      val a_ = a.map(NBitLong.bound(_, bits_))
      val b = IntLikeSet[Long, NBitLong](bits_, a_.map(NBitLong(bits_, _)))
      val c_ = if(c == Int.MinValue) 1 else c.abs
      (a_.size > c_) == b.sizeGreaterThan(c_)
  }
  property("bSar IntLike") = forAll{
    (a : Set[Long], bits : Int, toShift : Long) =>
      longBittedOp((bits_, x, s) => NBitLong.signContract(bits_, NBitLong.signExtend(bits_, NBitLong.signContract(bits_, x)) >> s), (x, s) => x.bSar(s.randomElement().getValue.intValue))(a, Set((toShift % implicitly[BoundedBits[Long]].bits).abs), bits)
  }
  /*XXX this sounds funny - how exactly does the default hashCode function work?
   * I thought it would work by reference. yet the following test passes?
   * Does it work by attribute?
   * (same for set above)
   */
  property("hashCode IntLike") = forAll{
    (a : Set[Long], bits : Int) =>
      val bits_ = NBitLong.boundBits(bits)
      val a_ = a.map(NBitLong.bound(_, bits_))
      val as = a_.toList
      val bs = util.Random.shuffle(as)
      val cs = util.Random.shuffle(as)
      //Build sets randomly - just to be sure
      val b = (IntLikeSet[Long, NBitLong](bits_) /: bs)((acc, x) => acc + NBitLong(bits_, x))
      val c = (IntLikeSet[Long, NBitLong](bits_) /: cs)((acc, x) => acc + NBitLong(bits_, x))
      b.hashCode == c.hashCode
  }
  property("mul IntLike") = forAll{
    (a : Set[Long], b : Set[Long], bits : Int, depths : Int) =>
      val longBits = implicitly[BoundedBits[Long]].bits
      val bits_ = NBitLong.boundBits(bits)
      val depths_ = NBitLong.boundBits(depths)
      val aBounded = a.map(NBitLong.bound(_, bits_))
      val bBounded = b.map(NBitLong.bound(_, bits_))
      val a_ = (IntLikeSet[Long, NBitLong](bits_) /: aBounded)((acc, x) => acc + NBitLong(bits_, x))
      val b_ = (IntLikeSet[Long, NBitLong](bits_) /: bBounded)((acc, x) => acc + NBitLong(bits_, x))
      val ref = cartesianProduct(aBounded, bBounded).map((x) => x._1 * x._2)
      //println("inputa_: " + a_ + "inputb_: " + b_ + ", bits: " + bits_ + ", depths: " + depths_)
      val us = a_.mul(depths_)(b_)
      val castIT = implicitly[Castable[(Int, Long), NBitLong]]
      val ref_ = ref.map((x : Long) => castIT((bits_ * 2, x)))
      val res = ref_.forall(us.contains)
      //if(!res) println("inputa_: " + a_ + "inputb_: " + b_ + ", bits: " + bits_ + ", us: " + us + ", ref: " + ref_ + ", result: " + res)
      res
  }
  property("range IntLike") = forAll{
    (lo : Long, hi : Long, bits : Int) =>
      val toTest = 100l
      val bits_ = NBitLong.boundBits(bits)
      val lo_ = NBitLong.bound(lo, bits_)
      val hi_ = NBitLong.bound(hi, bits_)
      val lo__ = lo_ min hi_
      val hi__ = lo_ max hi_
      val set = IntLikeSet.range[Long, NBitLong](NBitLong(bits_, lo__), NBitLong(bits_, hi__))
      val step = (hi__ - lo__).abs / toTest
      (true /: List.range(0l, toTest)){
        (acc, i) =>
          val test = i * step + lo__
          acc && set.contains(NBitLong(bits_, test))
      }
  }
/* [- AW -]
   Wichtigere Funktionalitaeten:
   teilmenge [- SCM -] DONE
   isFull [- SCM -] DONE
   isEmpty [- SCM -] DONE
   iterator [- SCM -] TEST: implicitly via ==?
   liste von elementen [- SCM -] DONE
   bitextract: first:last bits ausschneiden [- SCM -] DONE
   set mul set
   set plus set [- SCM -] DONE
   signextend [- SCM -] DONE
   zerofill [- SCM -] DONE
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
