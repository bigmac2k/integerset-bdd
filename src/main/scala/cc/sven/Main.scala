package cc.sven
import cc.sven.bdd.CBDD
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
  var durationNaive = 0L
  val test = forAll{
    (a : Set[Long], b : Long) =>
      (true) ==> {
        val longBits = implicitly[BoundedBits[Long]].bits
        val bits_ = longBits // (NBitLong.boundBits(bits) / 2) max 1

        val aBounded = a.map(x => NBitLong.bound(x, bits_)) // TODO

        val a_ = (IntLikeSet[Long, NBitLong](bits_) /: aBounded) ((acc, x) => acc + NBitLong(bits_, x))
        val b_ = NBitLong.bound(b, bits_)
        var start = System.nanoTime()
        val ref = cartesianProduct(aBounded, Set(b_)).map((x) => x._1 * x._2)
        durationRef += System.nanoTime() - start
        //println("inputa_: " + a_ + "inputb_: " + b_ + ", bits: " + bits_ + ", depths: " + depths_)

        start = System.nanoTime()
        val us = a_.mulSingleton(NBitLong(bits_, b))
        duration += System.nanoTime() - start

        start = System.nanoTime()
        val us2 = a_.mulSingleton3(NBitLong(bits_, b))
        duration2 += System.nanoTime() - start

        start = System.nanoTime()
        val us3 = a_.mulSingleton3(NBitLong(bits_, b))
        duration3 += System.nanoTime() - start

        start = System.nanoTime()
        val us4 = a_.mulSingleton4(NBitLong(bits_, b))
        duration4 += System.nanoTime() - start

        start = System.nanoTime()
        val ns = a_.mulNaive(IntLikeSet[Long, NBitLong](bits_) + NBitLong(bits_, b))
        durationNaive += System.nanoTime() - start

        val castIT = implicitly[Castable[(Int, Long), NBitLong]]
        val ref_ = ref.map((x: Long) => castIT((bits_, x)))
        val res = ref_.forall(us3.contains) && ref_.forall(us.contains) && ref_.forall(us2.contains) && ref_.forall(us4.contains)
        //if(!res) println("inputa_: " + a_ + "inputb_: " + b_ + ", bits: " + bits_ + ", us: " + us + ", ref: " + ref_ + ", result: " + res)
        res
      }
  }

  val testSingleton = forAll{
    (a : Set[Long], b : Long) =>
      val longBits = implicitly[BoundedBits[Long]].bits
      val bits_ = longBits // (NBitLong.boundBits(bits) / 2) max 1

      val aBounded = a.map(x => NBitLong.bound(x, bits_)) // TODO

      val a_ = (IntLikeSet[Long, NBitLong](bits_) /: aBounded) ((acc, x) => acc + NBitLong(bits_, x))
      val b_ = NBitLong.bound(b, bits_)
      var start = System.nanoTime()
      val ref = cartesianProduct(aBounded, Set(b_)).map((x) => x._1 + x._2)

      //println("inputa_: " + a_ + "inputb_: " + b_ + ", bits: " + bits_ + ", depths: " + depths_)
      val us = a_.plusSingleton(b)

      val castIT = implicitly[Castable[(Int, Long), NBitLong]]
      val ref_ = ref.map((x: Long) => castIT((bits_, x)))
      val res = ref_.forall(us.contains)
      println("inputa_: " + a_ + "inputb_: " + b_ + ", bits: " + bits_ + ", us: " + us + ", ref: " + ref_ + ", result: " + res)
      //if(!res) println("inputa_: " + a_ + "inputb_: " + b_ + ", bits: " + bits_ + ", us: " + us + ", ref: " + ref_ + ", result: " + res)
      res
  }

  def cartesianProduct[A, B](as : Set[A], bs : Set[B]) : Set[(A, B)] = for{
    a <- as
    b <- bs
  } yield (a, b)

  def main(args: Array[String]): Unit = {
    testSingleton.check(Test.Parameters.default.withMinSuccessfulTests(200))

    println(s"Reference: ${durationRef} ns / ${durationRef / 1000000} ms")
    println(s"Own: ${duration} ns / ${duration / 1000000} ms")
    println(s"Own (scala set): ${duration2} ns / ${duration2 / 1000000} ms")
    println(s"Own (bdd only): ${duration3} ns / ${duration3 / 1000000} ms")
    println(s"Own (bdd only + singleton ad): ${duration4} ns / ${duration4 / 1000000} ms")
    println(s"Naive: ${durationNaive} ns / ${durationNaive / 1000000} ms")
    println(s"Relative time own: ${duration / durationRef}x")
    println(s"Relative time own (scala set): ${duration2 / durationRef}x")
    println(s"Relative time own (bdd only): ${duration3 / durationRef}x")
    println(s"Relative time own (bdd only + singleton add): ${duration4 / durationRef}x")
    println(s"Relative time naive: ${durationNaive / durationRef}x")
    val bits = 64
    val set = Set(14L, 33L,134L) // 4L,5L,6L,7L, 8,32,33, 34,35,36,37,38,39,40,41,42,43,44,45,46,47) // generateSet()
    val bddSet = (IntLikeSet[Long, NBitLong](bits) /: set) ((acc, x) => acc + NBitLong(bits, x))
   // val (normal, ov) = CBDD.plusSingleton(bddSet.set.cbdd, IntSet.toBitVector(8L),64)
   // val result = new IntSet[Long](normal || ov)
    val r = bddSet.plusSingleton(5L)
    val op = NBitLong.bound(3L, bits)
    val ref = set.map(NBitLong.bound(_, bits)).map(_*op)
    val res = bddSet.mulSingleton3(NBitLong(bits, op))
    val castIT = implicitly[Castable[(Int, Long), NBitLong]]
    val correct = ref.map((x:Long)=>castIT((bits, x))).forall(res.contains)

    val x = bddSet.createStridedInterval(0, 7, 2)
    x
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
