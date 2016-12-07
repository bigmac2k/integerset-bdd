package cc.sven
import java.io.{BufferedWriter, File, FileWriter}
import java.nio.file.Paths

import cc.sven.bdd.{CBDD, False, Node, True}
import cc.sven.bounded.BoundedBits
import cc.sven.intset.IntSet
import cc.sven.tlike.{IntLikeSet, NBitLong, _}
import org.scalacheck.Prop.{BooleanOperators, forAll}
import org.scalacheck.Test

import scala.collection.AbstractSeq

object Main {
  var duration = 1L
  var duration2 = 0L
  var duration3 = 0L
  var duration4 = 0L
  var durationRef = 1L
  var durationNaive = 1L
  val test = forAll{
    (a : Set[Int], b : Int, k : Int, offset : Int, offset2 : Int, bits: Int) =>
      (b >0) ==> {
       // println("Test")
        val k_ = (k.abs % 10) max 1

        val interval = ((1 << k_) + offset) until ((1 << (k_ + 1)) + offset)
        val interval2 = ((1 << k_) + offset2) until ((1 << (k_ + 1)) + offset2)
        val longBits = implicitly[BoundedBits[Long]].bits
        val bits_ = 64 // 32 min ((NBitLong.boundBits(bits) / 2) max 1)

        val aBounded = (a ++ interval ++ interval2).map(x => NBitLong.bound(x.toLong, bits_)) // TODO

        val a_ = (IntLikeSet[Long, NBitLong](bits_) /: aBounded) ((acc, x) => acc + NBitLong(bits_, x))
        val b_ = NBitLong.bound(b, bits_)
        var start = System.nanoTime()
        val ref = cartesianProduct(aBounded, Set(b_.toLong)).map((x) => x._1 * x._2)
        durationRef += System.nanoTime() - start
        //println("inputa_: " + a_ + "inputb_: " + b_ + ", bits: " + bits_ + ", depths: " + depths_)

        start = System.nanoTime()
        //val us = a_.mulSingleton(NBitLong(bits_, b))
        duration += System.nanoTime() - start

        start = System.nanoTime()
        val us2 = a_.mulSingleton3(NBitLong(bits_, b))
        duration2 += System.nanoTime() - start

        start = System.nanoTime()
        val us3 = a_.mulSingleton3(NBitLong(bits_, b))
        duration3 += System.nanoTime() - start

        start = System.nanoTime()
        val us4 = a_.mulSingleton4(NBitLong(bits_, b_.toLong))
        duration4 += System.nanoTime() - start

        start = System.nanoTime()
        val ns = a_.mulNaive(IntLikeSet[Long, NBitLong](bits_) + NBitLong(bits_, b_.toLong))
        durationNaive += System.nanoTime() - start

        val castIT = implicitly[Castable[(Int, Long), NBitLong]]
        val ref_ = ref.map((x: Long) => castIT((bits_, x)))
        val res = ref_.forall(us4.contains) // ref_.forall(us3.contains) && ref_.forall(us.contains) && ref_.forall(us2.contains) &&
        //println("inputa_: " + a_ + "inputb_: " + b_ + ", bits: " + bits_ + ", us: " + us4 + ", ref: " + ref_ + ", result: " + res)
        if(!res) println("inputa_: " + a_.set + "inputb_: " + b_ + ", bits: " + bits_ + ", us: " + us4.set + ", ref: " + ref_ + ", result: " + res)

        c = c + 1
        res
      }
  }
  var c = 0

  val testSingleton = forAll{
    (a : Set[Long], b : Long, k : Int, offset : Long) =>
      val k_ = 3
      println(k_)
      val interval = ((1L << k_) + offset) until ((1L << (k_ + 1)) + offset)
      val longBits = implicitly[BoundedBits[Long]].bits
      val bits_ = longBits // (NBitLong.boundBits(bits) / 2) max 1

      val aBounded = (a ++ interval).map(x => NBitLong.bound(x, bits_)) // TODO

      val a_ = (IntLikeSet[Long, NBitLong](bits_) /: aBounded) ((acc, x) => acc + NBitLong(bits_, x))
      val b_ = NBitLong.bound(b, bits_)
      var start = System.nanoTime()
      val ref = cartesianProduct(aBounded, Set(b_)).map((x) => x._1 + x._2)

      //println("inputa_: " + a_ + "inputb_: " + b_ + ", bits: " + bits_ + ", depths: " + depths_)
      val us = a_.plusSingleton(b)

      val castIT = implicitly[Castable[(Int, Long), NBitLong]]
      val ref_ = ref.map((x: Long) => castIT((bits_, x)))
      val res = ref_.forall(us.contains) && us.forall(ref_.contains)
     // println("inputa_: " + a_ + "inputb_: " + b_ + ", bits: " + bits_ + ", us: " + us + ", ref: " + ref_ + ", result: " + res)
      //if(!res) println("inputa_: " + a_ + "inputb_: " + b_ + ", bits: " + bits_ + ", us: " + us + ", ref: " + ref_ + ", result: " + res)
      res
  }

  val testBinaryMul = forAll{
    (a:Long, b:Long) =>
      val a_ = a
      val b_ = b
      val aBools = IntSet.toBitVector(a_)
      val bBools = IntSet.toBitVector(b_)
      val r = IntSet.fromBitVector[Long](mulBinary(aBools, bBools))
      val ref = a_ * b_
      val res = r == ref
      if (!res) println("inputa_: " + a_ + "inputb_: " + b_ + ", , us: " + r + ", ref: " + ref + ", result: " + res)
      res
  }

  var stridedNaive = 1L
  var stridedOwn = 1L
  val testStridedInterval = forAll {
    (start: Int, count: Int, stride: Int) =>

      val stride_ = stride.abs.toLong max 1L
      val start_ = (Long.MaxValue - 1 min start.toLong)
      val count_ = (Long.MaxValue / stride_ * 2) % 100000
      (count_ > 0) ==> {
        val bits = 64
        println("start: " + start_ + ", count: " + count_ + ", stride: " + stride_)
        val expected = for (i <- 0L to (count_ - 1)) yield start_ + i * stride_
        val expected_ = expected.map(x => NBitLong(bits, x)).toSet

        var start = System.nanoTime()
        println("Started own")
        val resBdd = CBDD.constructStridedInterval(start_, count_, stride_, bits)

        val us = new IntLikeSet[Long, NBitLong](bits, new IntSet[Long](resBdd))
        println("Finished own")
        stridedOwn += System.nanoTime() - start

        start = System.nanoTime()
        println("Started naive")
       // val a_ = IntSet(expected.toSet)
        println("Finished naive")
        stridedNaive += System.nanoTime() - start

        val res = expected_.forall(us.contains) && us.forall(expected_.contains)
        if (!res) println("Wrong: start: " + start_ + ", count: " + count_ + ", stride: " + stride_)
        res
      }
  }

  def cartesianProduct[A, B](as : Set[A], bs : Set[B]) : Set[(A, B)] = for{
    a <- as
    b <- bs
  } yield (a, b)

  def mulBinary(x : List[Boolean], y : List[Boolean]) : List[Boolean] = {
    def add(x : List[Boolean], y : List[Boolean]) : (Boolean, List[Boolean]) = (x,y) match {
      case (List(a), List(b)) => (a && b, List(a != b))
      case (a::as, b::bs) =>
        val (c, rs) = add(as, bs)
        ((c && a) || (c && b) || (a && b), (a!=(c!=b)) :: rs)
    }
    val l = 2 * (x.length max y.length)
    val x_ = List.fill(l - x.length)(x.head) ++ x
    val y_ = List.fill(l - y.length)(y.head) ++ y
    var r = List.fill(l)(false)
    for {i <- x_.indices} {
      if (x_(l-i-1)) {
        val shifted = y_.drop(i) ++ List.fill(i)(false)
        r = add(r, shifted)._2
      }
    }
    r.drop(l / 2)
  }

  def printBdd(bdd: CBDD): String = {
    def helper(bdd: CBDD, depth: Int) : String = "".padTo(depth*2,' ') + (bdd match {
      case True =>  "True"
      case False => "False"
      case (Node(s, uset)) => s"o\n  ${helper(s, depth + 1)}\n  ${helper(uset, depth+1)}"
    })
    helper(bdd, 0)
  }

  def benchmarkSuiteHeights(folder: String, start: Long, lengths: AbstractSeq[Long], strides: AbstractSeq[Long], heights: Range): Unit = {
    for (stride <- strides) {
      for (count <- lengths) {
        val p = Paths.get(folder)
        val file = p.resolve(s"benchmark_start_${start}_length_${count}_stride_${stride}_height_from_${heights.start}_to_${heights.end}.csv").toFile
        val bw = new BufferedWriter(new FileWriter(file))
        for (height <- heights) {
          println(s"Start: $start, Stride: $stride, Length: $count")
          val (tree, rcount) = constructStridedInterval(start, count, stride, height)
          bw.write(s"$height;$rcount\n")
          bw.flush()
        }
        bw.close()
      }
    }
  }

  def benchmarkSuiteStarts(folder: String, starts: IndexedSeq[Long], lengths: AbstractSeq[Long], strides: AbstractSeq[Long], height: Int): Unit = {
    for (stride <- strides) {
      for (count <- lengths) {
        val p = Paths.get(folder)
        val file = p.resolve(s"benchmark_length_${count}_stride_${stride}_start_from_${starts.min}_to_${starts.max}.csv").toFile
        val bw = new BufferedWriter(new FileWriter(file))
        for (start <- starts) {
          println(s"Start: $start, Stride: $stride, Length: $count")
          val (tree, rcount) = constructStridedInterval(start, count, stride, height)
          bw.write(s"$start;$rcount\n")
          bw.flush()
        }
        bw.close()
      }
    }
  }

  def benchmarkSuiteLengths(folder: String, start: Long, lengths: Range, strides: AbstractSeq[Long], height: Int): Unit = {
    for (stride <- strides) {
      val p = Paths.get(folder)
      val file = p.resolve(s"benchmark_start_${start}_stride_${stride}_length_from_${lengths.start}_to_${lengths.end}_step_${lengths.step}.csv").toFile
      val bw = new BufferedWriter(new FileWriter(file))
      for (count <- lengths) {
        println(s"Stride: $stride, Length: $count")
        val (tree, rcount) = constructStridedInterval(start, count, stride, height)
        bw.write(s"$count;$rcount\n")
        bw.flush()
      }
      bw.close()
    }
  }

  def benchmarkSuiteStrides(folder: String, start: Long, lengths: AbstractSeq[Long], strides: IndexedSeq[Long], height: Int): Unit = {
    for (length <- lengths) {
      val p = Paths.get(folder)
      val file = p.resolve(s"benchmark_start_${start}_length_${length}_stride_from_${strides.min}_to_${strides.max}.csv").toFile
      val bw = new BufferedWriter(new FileWriter(file))


      for (stride <- strides) {
        println(s"Stride: $stride, Length: $length")
        val (tree, rcount) = constructStridedInterval(start, length, stride, height)
        bw.write(s"$stride;$rcount\n")
        bw.flush()
      }
      bw.close()
    }
  }

  def benchmark(filename: String) = {
    val file = new File(filename)
    val bw = new BufferedWriter(new FileWriter(file))

    val r = scala.util.Random
    val x = 1 to 64
    x
    val end = 64
    val loopCount = 500
    val start = r.nextInt() / 2
    val stride = 1L << 10
    for (n <- 32 to(end, 1)) {

      val n_ = n //1 << n
      println(n_)
      var duration = 0L
      var durationNaive = 0L
      //for (i <- 1 to loopCount) {
        //val start = r.nextInt()
        var s = System.nanoTime()

        val (tree, count) = constructStridedInterval(start, 10000, stride, n_)
        duration += System.nanoTime() - s
      /*  s = System.nanoTime()
        val expected = (for (i <- 0L to (n - 1)) yield start + i * stride).toSet
        val expected_ = expected.toSet


        val a_ = IntSet(expected)

        durationNaive += System.nanoTime() - s

        println(durationNaive)*/
     // }
      bw.write(s"$n_;${count}\n")
      bw.flush()
    }
    bw.close()
  }

  def main(args: Array[String]): Unit = {
    val r = scala.util.Random
    //benchmarkSuiteLengths("benchmarks", r.nextInt(1 << 12), 0 to (2000000, 50000), List(1L << 5, 1L << 10, 1L << 20), 64)
   // benchmarkSuiteStrides("benchmarks", r.nextInt(1 << 12).abs, List(10000L, 20000L), (0 to (100000, 1000)).map(_.toLong), 64)
    //benchmark("benchmark_height_numrecursion.csv")
    //Main.testStridedInterval.check(Test.Parameters.defaultVerbose.withMinSuccessfulTests(250))

    println(s"Naive: ${stridedNaive} ns / ${stridedNaive / 1000000} ms")
    println(s"Own: ${stridedOwn} ns / ${stridedOwn / 1000000} ms (${stridedOwn.toDouble / stridedNaive}x ")

    val test = CBDD.constructStridedInterval(-30, 7, 6, 64)
    // val t = new IntSet[Long](test)
    println(s"Reference: ${durationRef} ns / ${durationRef / 1000000} ms")
    println(s"Own: ${duration} ns / ${duration / 1000000} ms")
    println(s"Own (scala set): ${duration2} ns / ${duration2 / 1000000} ms")
    println(s"Own (bdd only): ${duration3} ns / ${duration3 / 1000000} ms")
    println(s"Own (bdd only + singleton ad): ${duration4} ns / ${duration4 / 1000000} ms")
    println(s"Naive: ${durationNaive} ns / ${durationNaive / 1000000} ms")
    println(s"Relative time own: ${duration / durationRef}x (${duration / durationNaive}x)")
    println(s"Relative time own (scala set): ${duration2 / durationRef}x (${duration2 / durationNaive}x)")
    println(s"Relative time own (bdd only): ${duration3 / durationRef}x (${duration3 / durationNaive}x)")
    println(s"Relative time own (bdd only + singleton add): ${duration4 / durationRef}x (${duration4 / durationNaive}x)")
    println(s"Relative time naive: ${durationNaive / durationRef}x")

    val a = List(false,true,true)
    val b = List(false,true, true)
    val xy = mulBinary(a,b)
    val bits = 64
    val k = 4
    val interval = 1L << k until 1L << (k+1)
    var set = Set (20L, 21L, 16L, 17L, 18L, 19L, 0L, 1L, 2L, 3L, 4L, 5L, 6L, 7L, 8L, 9L, 10L, 11L, 12L, 13L, 14L, 15L)//Set(-9223372036854775806L, -9223372036854775805L) // 4L,5L,6L,7L, 8,32,33, 34,35,36,37,38,39,40,41,42,43,44,45,46,47) // generateSet()
    //set ++= interval
    val bddSet = (IntLikeSet[Long, NBitLong](bits) /: set) ((acc, x) => acc + NBitLong(bits, NBitLong.bound(x, bits)))
   // val (normal, ov) = CBDD.plusSingleton(bddSet.set.cbdd, IntSet.toBitVector(8L),64)
   // val result = new IntSet[Long](normal || ov)
//    val r = bddSet.plusSingleton(5L).set.toList.sorted
    val op = NBitLong.bound(4034120, bits)
    val ref = set.map(NBitLong.bound(_, bits)).map(_*op)
    //val res = bddSet.mulSingleton4(NBitLong(bits, op))
    //println(res)
    val castIT = implicitly[Castable[(Int, Long), NBitLong]]
    //val correct = ref.map((x:Long)=>castIT((bits, x))).forall(res.contains)

    val x = bddSet.createStridedInterval(0, 7, 2)

  }

def constructStridedInterval(start: Long, count: Long, stride : Long, height : Int) : (CBDD, Long) = {
  if (stride == 0) return (CBDD(IntSet.toBitVector(start)), 0)
  val stride_ = stride.abs
  var recursionCount = 0L
  def helper2(toBeConsumed: Long, h: Long, length: Long): (CBDD, Long, Long) = {
    recursionCount = recursionCount + 1
    require(toBeConsumed >= 0)
    if (length <= 0) return (False, toBeConsumed, 0)
    if (h == 0 && toBeConsumed == 0) return (True, stride_ - 1, 1)

    val remaining = if (h < 63) toBeConsumed - (1L << h) else -1 // overflow, if h>62 we can consume any 0<=x<=long.MaxValue

    if (remaining < 0L) {
      val (bddF, leftF, countF) = helper2(toBeConsumed, h - 1, length)
      val (bddT, leftT, countT) = helper2(leftF, h - 1, length - countF)
      (Node(bddT, bddF), leftT, countF + countT)
    } else {
      (False, remaining, 0)
    }
  }

  val maxCount = count // (end - start).abs / stride_ + 1
  val start_ = start // Math.min(start, end)
  if (start < 0) {
    val start__ = start_ + (1L << height - 2) + (1L << height - 2) // not in right subtree. TODO wrap around
    val (res, left, c) = helper2(start__, height - 1, maxCount)
    if (c < maxCount) {
      val (res2, left2, c2) = helper2(left, height - 1, maxCount - c)
      (Node(res, res2), recursionCount)
    } else {
      (Node(res, False), recursionCount)
    }

  } else {
    val (res, left, c) = helper2(start_, height, maxCount)
    (res, recursionCount)
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
}
