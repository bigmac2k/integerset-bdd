package cc.sven
import java.io.{BufferedWriter, File, FileWriter}
import java.nio.file.Paths

import cc.sven.bdd._
import cc.sven.bounded.BoundedBits
import cc.sven.interval._
import cc.sven.intset.IntSet
import cc.sven.tlike.{IntLikeSet, NBitLong, _}
import org.scalacheck.Prop.{BooleanOperators, forAll}
import org.scalacheck.Test

import scala.collection.{AbstractSeq, mutable}

object Main {
  val testSingleton = forAll{
    (a : Set[Int], b : Int, k : Int, offset : Int, offset2 : Int, bits: Int) =>
      val longBits = implicitly[BoundedBits[Long]].bits
      val bits_ = 32 min ((NBitLong.boundBits(bits) / 2) max 1)

      val aBounded = a.map(x => NBitLong.bound(x.toLong, bits_))

      val a_ = (IntLikeSet[Long, NBitLong](bits_) /: aBounded) ((acc, x) => acc + NBitLong(bits_, x))
      val b_ = NBitLong.bound(if (b==0) 5 else b, bits_)
      var start = System.nanoTime()
      val ref = cartesianProduct(aBounded, Set(b_)).map((x) => x._1 * x._2)
      val us = a_.mulSingleton(NBitLong(bits_, b_))

      val castIT = implicitly[Castable[(Int, Long), NBitLong]]
      val ref_ = (IntLikeSet[Long, NBitLong](2*bits_) /: ref) ((acc, x) => acc + NBitLong(2*bits_, x))
      val res = ref_ subsetOf us
      if(!res) println("Fail inputa_: " + a_ + "inputb_: " + b_ + ", bits: " + bits_ + ", us: " + us + ", ref: " + ref + ", result: " + res)
      res
  }

  val testGeneralDepth = forAll{
    (a : Set[Long], b : Set[Long], bits : Int, depths : Int, findBounds: Boolean) =>
      val longBits = implicitly[BoundedBits[Long]].bits
      val bits_ = (NBitLong.boundBits(bits) / 2) max 1
      val depths_ = NBitLong.boundBits(depths).abs % bits_
      val aBounded = a.map(NBitLong.bound(_, bits_))
      val bBounded = b.map(NBitLong.bound(_, bits_))
      val a_ = (IntLikeSet[Long, NBitLong](bits_) /: aBounded)((acc, x) => acc + NBitLong(bits_, x))
      val b_ = (IntLikeSet[Long, NBitLong](bits_) /: bBounded)((acc, x) => acc + NBitLong(bits_, x))
      val ref = cartesianProduct(aBounded, bBounded).map((x) => x._1 * x._2)
      //println("inputa_: " + a_ + "inputb_: " + b_ + ", bits: " + bits_ + ", depths: " + depths_)
      val us = a_.mulPredicate(DepthPredicate(depths_))(findBounds)(b_)
      val castIT = implicitly[Castable[(Int, Long), NBitLong]]
      val ref_ = ref.map((x : Long) => castIT((bits_ * 2, x)))
      val res = ref_.forall(us.contains)
      if(!res) println("Wrong: inputa_: " + a_ + "inputb_: " + b_ + ", bits: " + bits_ + ", us: " + us + ", ref: " + ref_ + ", result: " + res)
      res
  }

  val testGeneralPrecision= forAll{
    (a : Set[Long], b : Set[Long], bits : Int, precision : Int, findBounds: Boolean) =>
      val longBits = implicitly[BoundedBits[Long]].bits
      val bits_ = (NBitLong.boundBits(bits) / 2) max 1
      val p = (precision % 100).abs
      val precision_ = (if (p == 0) 50 else p).abs.toDouble / 100
      val aBounded = a.map(NBitLong.bound(_, bits_))
      val bBounded = b.map(NBitLong.bound(_, bits_))
      val a_ = (IntLikeSet[Long, NBitLong](bits_) /: aBounded)((acc, x) => acc + NBitLong(bits_, x))
      val b_ = (IntLikeSet[Long, NBitLong](bits_) /: bBounded)((acc, x) => acc + NBitLong(bits_, x))
      val ref = cartesianProduct(aBounded, bBounded).map((x) => x._1 * x._2)
      //println("inputa_: " + a_ + "inputb_: " + b_ + ", bits: " + bits_ + ", depths: " + precision_)
      val us = a_.mulPredicate(PrecisionPredicate(precision_))(findBounds)(b_)
      val castIT = implicitly[Castable[(Int, Long), NBitLong]]
      val ref_ = ref.map((x : Long) => castIT((bits_ * 2, x)))
      val res = ref_.forall(us.contains)
      if(!res) println("inputa_: " + a_ + "inputb_: " + b_ + ", bits: " + bits_ + ", us: " + us + ", ref: " + ref_ + ", result: " + res)
      res
  }

  val testStridedInterval = forAll {
    (stride: Int, start: Int, end: Int) =>
      val stride_ = math.max(2L,stride.abs)
      val start_ = math.min(start.abs,end.abs) // (Long.MaxValue - 1 min start.toLong)
    val end_ = start.abs max end.abs
      val count_ = ((end_ - start_) / stride_) % 100000
      (count_ > 0) ==> {
        val bits = 64

        val expected = for (i <- 0L to (count_ - 1)) yield start_ + i * stride_
        val expected_ = expected.map(x => NBitLong(bits, x)).toSet
        var start = System.nanoTime()
        val resBdd = CBDD.constructStridedInterval(start_, count_, stride_, bits)

        val us = new IntLikeSet[Long, NBitLong](bits, new IntSet[Long](resBdd))

        val res = expected_.forall(us.contains) && us.forall(expected_.contains)
        if (!res) println("Wrong: start: " + start_ + ", end: " + end_ + ", count: " + count_ + ", stride: " + stride_)
        res
      }
  }

  val testStrideConversionFromStrided = forAll {
    (start: Int, count: Int, stride: Int) =>
      val stride_ = 2L max stride.abs % 10000
      val start_ = start.abs max 0 // (Long.MaxValue - 1 min start.toLong)

      val count_ = 2L max count.abs % 100000 min ((1L << 32) - start_) / stride_

      (count_ > 0) ==> {
        val end__ = start_ + (count_ - 1) * stride_

        val resBdd = CBDD.constructStridedInterval(start_, count_, stride_, 32)

        val (start2, stride2, end2) = findStrideMemo(resBdd, 32)

        val res = start2 == start_ && stride2 == stride_ && end2 == end__
        if (!res) println("Wrong: start: " + start_ + ", stride: " + stride_ + ", end: " + end__ + ", start: " + start2 + ", stride: " + stride2 + ", end2: " + end2)
        res
      }
  }

  val testStrideConversionFromGeneral= forAll {
    (set: Set[Int], bits: Int) =>
      val longBits = implicitly[BoundedBits[Long]].bits
      val bits_ = 32 // (NBitLong.boundBits(bits) / 2) max 1
    val aBounded = set.map(NBitLong.bound(_, bits_).toInt)
      val a_ = (IntLikeSet[Int, Int](bits_) /: aBounded)((acc, x) => acc + (x.abs & ((1<<31) -1)))

      val x = a_.toStridedInterval

      val res = x match {
        case EmptyStridedIval => a_.isEmpty
        case FilledStridedIval(stride, start, end) => a_.forall(v => (v - start) % stride == 0 && v >= start && v <= end)
      }

      if (!res) println(s"Wrong: input: $a_, interval: $x")
      res
  }

  def combineIntervals(intervals: Set[Interval[Long]], bits: Int): IntLikeSet[Long, NBitLong] = {
    (IntLikeSet[Long, NBitLong](bits) /: intervals){
      case (acc, EmptyIval) => acc
      case (acc, FilledIval(lo, hi)) => acc union IntLikeSet.range[Long, NBitLong](bits min 64, lo, hi).changeBitWidth(bits)
    }
  }

  val testIntervalApproximationDepth = forAll {
    (a: Set[Long], bits: Int, depth: Int, findBounds: Boolean) =>
      val longBits = implicitly[BoundedBits[Long]].bits
      val bits_ = 32 // (NBitLong.boundBits(bits) / 2) max 1
      val depth_ = NBitLong.boundBits(depth).abs % bits_
      val aBounded = a.map(NBitLong.bound(_, bits_))
      val a_ = (IntLikeSet[Long, NBitLong](bits_) /: aBounded)((acc, x) => acc + NBitLong(bits_, x))
      val approximation = a_.toIvalSetPredicate(DepthPredicate(depth_))(findBounds)
      val combined = combineIntervals(approximation, bits_)
      val res = a_.forall(combined.contains)
      if (!res) {
        println(a_ intersect !combined)
        println(a_.set.cbdd.depth, combined.set.cbdd.depth)
        println("inputa_: " + a_ + "depth: " + depth_ + ", bits: " + bits_ + ", us: " + combined + ", result: " + res)
      }
      res
  }




  def cartesianProduct[A, B](as : Set[A], bs : Set[B]) : Set[(A, B)] = for{
    a <- as
    b <- bs
  } yield (a, b)

  def printBdd(bdd: CBDD): String = {
    def helper(bdd: CBDD, depth: Int) : String = "".padTo(depth*2,' ') + (bdd match {
      case True =>  "True"
      case False => "False"
      case (Node(s, uset)) => s"o\n  ${helper(s, depth + 1)}\n  ${helper(uset, depth+1)}"
    })
    helper(bdd, 0)
  }

  def toIntSet(a: Set[Int]): IntLikeSet[Long, NBitLong] = {
    val aBounded = a.map(x => NBitLong.bound(x.toLong, 32))
    (IntLikeSet[Long, NBitLong](32) /: aBounded) ((acc, x) => acc + NBitLong(32, x))
  }

  def mul(a: Set[Int], y: Int, bits: Int): IntLikeSet[Long, NBitLong] = {

    val aBounded = a.map(x => NBitLong.bound(x.toLong , bits))
    val a_ = (IntLikeSet[Long, NBitLong](bits) /: aBounded) ((acc, x) => acc + NBitLong(bits, x))
    val b_ = NBitLong.bound(y.toLong, bits)
    var start = System.nanoTime()


    //println("inputa_: " + a_ + "inputb_: " + b_ + ", bits: " + bits_ + ", depths: " + depths_)
    val us = a_.mulSingleton(NBitLong(bits, b_.toLong))

    val castIT = implicitly[Castable[(Int, Long), NBitLong]]

    us
  }

  def main(args: Array[String]): Unit = {
    val x = mul(Set(0,1,2,3,6,-10), -10, 32)
    println(x)
    /*
    Main.testGeneralDepth.check(Test.Parameters.defaultVerbose.withMinSuccessfulTests(100))
    Main.testGeneralPrecision.check(Test.Parameters.defaultVerbose.withMinSuccessfulTests(100))
    Main.testStrideConversionFromGeneral.check(Test.Parameters.defaultVerbose.withMinSuccessfulTests(100))
    Main.testStrideConversionFromStrided.check(Test.Parameters.defaultVerbose.withMinSuccessfulTests(100))

    Main.testStridedInterval.check(Test.Parameters.defaultVerbose.withMinSuccessfulTests(100)) */
    Main.testGeneralDepth.check(Test.Parameters.defaultVerbose.withMinSuccessfulTests(1000))
  }

  def intervalSet(depth: Int): CBDD = {
    if (depth <= 0) {
      True
    } else {
      Node(False, intervalSet(depth - 1))
    }
  }

  def memoize[I, O](f: I => O): I => O = new mutable.HashMap[I, O]() {
    override def apply(key: I) = {
      getOrElseUpdate(key, f(key))
    }
  }

  def findStrideMemo(bdd: CBDD, height: Int): (Long, Long, Long) = {
    def gcd(a: Long, b: Long): Long = if (b == 0) {
      a
    } else {
      gcd(b, a % b)
    }
    lazy val helperMemo: ((CBDD, Int)) => (Long, Long, Long, Boolean) = memoize[(CBDD, Int),(Long, Long, Long, Boolean)] {
      case (True, 0)  => (0L, 0L, 0L, true)
      case (True, h)  => (0L, 1L, 0L, true)
      case (False, h) => (1L << h, 0L, 0L, false)
      case (Node(set, uset), h) => {
        val (fLeft, fStride, fRight, fContainsTrue) = helperMemo(uset, h - 1)
        if (fStride == 1) {
          (fLeft, fStride, fRight, fContainsTrue)
        } else {
          val (tLeft, tStride, tRight, tContainsTrue) = helperMemo(set, h - 1)

          if (!tContainsTrue) {
            (tLeft + tRight + fLeft, fStride, fRight, fContainsTrue)
          } else if (!fContainsTrue) {
            (tLeft, tStride, tRight + fLeft + fRight, tContainsTrue)
          } else {
            (tLeft, gcd(gcd(fStride, tStride), tRight + fLeft + 1), fRight, tContainsTrue || fContainsTrue)
          }
        }
      }
    }
    val (left, stride, right, _) = helperMemo((bdd, height))
    if (stride != 1) {
      (right, if (stride <= 1) 1 else stride, (1L << height) - left - 1)
    } else {
      (right, 1, IntSet.fromBitVector[Long](bdd.trueMost.get))
    }

  }

  def generateSet() = {
    val r = scala.util.Random
    val length = r.nextInt(500)

    val s = for {
      i <- 1 to length
    } yield r.nextLong()
    s.toSet
  }

  def fastGcd(nums: List[Long]): List[Long] = {
    def gcd(a: Long, b: Long): Long = if (b == 0) {
      a
    } else {
      gcd(b, a % b)
    }

    val until = new Array[Long](nums.length + 1)
    val from = new Array[Long](nums.length + 1)
    until(0) = 0
    from(nums.length) = 0
    for (i <- 1 to nums.length) {
      until(i) = gcd(until(i-1), nums(i-1))
      from(nums.length - i) = gcd(from(nums.length - i + 1), nums(nums.length - i))
    }
    (1 to nums.length).map(i => gcd(until(i-1), from(i))).toList
  }
}



