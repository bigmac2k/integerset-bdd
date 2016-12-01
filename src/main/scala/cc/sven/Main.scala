package cc.sven
import cc.sven.bdd.{CBDD, False, Node, True}
import cc.sven.bounded.BoundedBits
import cc.sven.intset.IntSet
import cc.sven.tlike.{IntLikeSet, _}
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
    (a : Set[Long], b : Int,k : Int, offset : Long, bits: Int) =>
      (b >0) ==> {
        val k_ = (k.abs % 10) max 1

        val interval = ((1L << k_) + offset) until ((1L << (k_ + 1)) + offset)
        val longBits = implicitly[BoundedBits[Long]].bits
        val bits_ = 64 // 32 min ((NBitLong.boundBits(bits) / 2) max 1)

        val aBounded = (a ++ interval).map(x => NBitLong.bound(x, bits_)) // TODO

        val a_ = (IntLikeSet[Long, NBitLong](bits_) /: aBounded) ((acc, x) => acc + NBitLong(bits_, x))
        val b_ = NBitLong.bound(b, bits_)
        var start = System.nanoTime()
        val ref = cartesianProduct(aBounded, Set(b_)).map((x) => x._1 * x._2)
        durationRef += System.nanoTime() - start
        //println("inputa_: " + a_ + "inputb_: " + b_ + ", bits: " + bits_ + ", depths: " + depths_)
        /*
        start = System.nanoTime()
        //val us = a_.mulSingleton(NBitLong(bits_, b))
        duration += System.nanoTime() - start

        start = System.nanoTime()
        val us2 = a_.mulSingleton3(NBitLong(bits_, b))
        duration2 += System.nanoTime() - start

        start = System.nanoTime()
        val us3 = a_.mulSingleton3(NBitLong(bits_, b))
        duration3 += System.nanoTime() - start
        */
        start = System.nanoTime()
        val us4 = a_.mulSingleton4(NBitLong(bits_, b))
        duration4 += System.nanoTime() - start

        start = System.nanoTime()
        val ns = a_.mulNaive(IntLikeSet[Long, NBitLong](bits_) + NBitLong(bits_, b))
        durationNaive += System.nanoTime() - start

        val castIT = implicitly[Castable[(Int, Long), NBitLong]]
        val ref_ = ref.map((x: Long) => castIT((bits_, x)))
        val res = ref_.forall(us4.contains) // ref_.forall(us3.contains) && ref_.forall(us.contains) && ref_.forall(us2.contains) &&
        //println("inputa_: " + a_ + "inputb_: " + b_ + ", bits: " + bits_ + ", us: " + us4 + ", ref: " + ref_ + ", result: " + res)
        if(!res) println("inputa_: " + a_ + "inputb_: " + b_ + ", bits: " + bits_ + ", us: " + us4 + ", ref: " + ref_ + ", result: " + res)
        res
      }
  }

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

  def main(args: Array[String]): Unit = {
    Main.test.check(Test.Parameters.defaultVerbose.withMinSuccessfulTests(200))
    val test = construct(108, 54678, 35, 64)
    val t = new IntSet[Long](test)
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
    var set = Set (-1L, -9223372036854775800L, -9223372036854775799L, -9223372036854775798L, -9223372036854775797L, -9223372036854775796L, -9223372036854775795L, 922337203685477579L, -9223372036854775793L, -9223372036854775808L, 9223372036854775807L, 4156699499875519015L, 203650941454430939L, 0L)//Set(-9223372036854775806L, -9223372036854775805L) // 4L,5L,6L,7L, 8,32,33, 34,35,36,37,38,39,40,41,42,43,44,45,46,47) // generateSet()
    // set ++= interval
    val bddSet = (IntLikeSet[Long, NBitLong](bits) /: set) ((acc, x) => acc + NBitLong(bits, NBitLong.bound(x, bits)))
   // val (normal, ov) = CBDD.plusSingleton(bddSet.set.cbdd, IntSet.toBitVector(8L),64)
   // val result = new IntSet[Long](normal || ov)
    val r = bddSet.plusSingleton(5L).set.toList.sorted
    val op = NBitLong.bound(3L, bits)
    val ref = set.map(NBitLong.bound(_, bits)).map(_*op)
    val res = bddSet.mulSingleton4(NBitLong(bits, op))
    println(res)
    val castIT = implicitly[Castable[(Int, Long), NBitLong]]
    val correct = ref.map((x:Long)=>castIT((bits, x))).forall(res.contains)

    val x = bddSet.createStridedInterval(0, 7, 2)

  }

  def construct(start: Long, end: Long, stride : Long, depth : Int) : CBDD = {
    val maxCount = (end - start) / stride + 1
    def helper(num : Long, d : Int, countLeft : Long) : (CBDD, Long, Long) = {
      if (countLeft <= 0) return (False, num, 0)
      val count = if (d >= 64) Long.MaxValue else 1L << (d - 1) // max number leaves in subtree
      if (d == 0) return (True, stride - 1, 1)
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
    val (res, left, c) = helper(start, depth, maxCount)
    res
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
