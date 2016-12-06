package cc.sven
import java.io.{BufferedWriter, File, FileWriter}

import cc.sven.bdd.{CBDD, False, Node, True}
import cc.sven.bounded.BoundedBits
import cc.sven.intset.IntSet
import cc.sven.tlike.{IntLikeSet, NBitLong, _}
import org.scalacheck.Prop.{BooleanOperators, forAll}
import org.scalacheck.Test

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

  def benchmark(filename: String) = {
    val file = new File(filename)
    val bw = new BufferedWriter(new FileWriter(file))

    val r = scala.util.Random

    val end = 30
    val loopCount = 500
    val start = 0 //r.nextInt()
    for (n <- 1 to(end, 1)) {

      val n_ = 1 << n
      println(n_)
      var duration = 0L
      var durationNaive = 0L
      for (i <- 1 to loopCount) {
        val start = r.nextInt()
        var s = System.nanoTime()

        val c = CBDD.constructStridedInterval(start, 10000, n_, 64)
        duration += System.nanoTime() - s
      /*  s = System.nanoTime()
        val expected = (for (i <- 0L to (n - 1)) yield start + i * stride).toSet
        val expected_ = expected.toSet


        val a_ = IntSet(expected)

        durationNaive += System.nanoTime() - s

        println(durationNaive)*/
      }
      bw.write(s"$n;${duration.toDouble/loopCount};${durationNaive.toDouble/loopCount}\n")
      bw.flush()
    }
    bw.close()
  }

  def main(args: Array[String]): Unit = {
   // benchmark("benchmark.csv")
    Main.testStridedInterval.check(Test.Parameters.defaultVerbose.withMinSuccessfulTests(250))

    println(s"Naive: ${stridedNaive} ns / ${stridedNaive / 1000000} ms")
    println(s"Own: ${stridedOwn} ns / ${stridedOwn / 1000000} ms (${stridedOwn.toDouble / stridedNaive}x ")

    val test = CBDD.constructStridedInterval(3, 6, 5, 64)
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

  def construct(start: Long, end: Long, stride : Long, depth : Int) : CBDD = {
    val stride_ = stride.abs

    def helper(num : Long, d : Int, countLeft : Long) : (CBDD, Long, Long) = {
      if (countLeft <= 0) return (False, num, 0)
      val count = if (d >= 64) Long.MaxValue else 1L << (d - 1) // max number leaves in subtree
      if (d == 0) return (True, stride_ - 1, 1)
      if (num < count) { // at least one leaf in right subtree
        val (bddF, leftF, countF) = helper(num, d - 1, countLeft)
        if (leftF < count && countF < countLeft) { // go into left subtree
          val (bddT, leftT, countT) = helper(leftF, d - 1, countLeft - countF)
          (Node(bddT, bddF), leftT, countF + countT)
        } else {
          (Node(False, bddF), leftF - count, countF)
        }
      } else if (num - count < count) {
        val num_ = num - count
        val (bddT, leftT, countT) = helper(num_, d - 1, countLeft)
        (Node(bddT, False), leftT, countT)
      } else {
        (False, num - 2 * count, 0)
      }
    }
    val maxCount = (end - start).abs / stride_ + 1
    val start_ = Math.min(start, end)
    if (start < 0) {
      val start__ = start_ + (1L << depth - 1) // not in right subtree
      val (res, left, c) = helper(start__, depth - 1, maxCount)
      if (c < maxCount) {
        val (res2, left2, c2) = helper(left, depth - 1, maxCount - c)
        Node(res, res2)
      } else {
        Node(res, False)
      }

    } else {
      val (res, left, c) = helper(start_, depth, maxCount)
      res
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
