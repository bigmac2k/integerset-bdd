package cc.sven.misc

import cc.sven.tlike._
import cc.sven.bounded._

object Misc {
  def cartesianProduct[A, B](as : Set[A], bs : Set[B]) : Set[(A, B)] = for{
    a <- as
    b <- bs
  } yield (a, b)
  
  def longBittedOp(normalop : (Int, Long, Long) => Long, intlikeop : (IntLikeSet[Long, NBitLong], IntLikeSet[Long, NBitLong]) => IntLikeSet[Long, NBitLong]) : (Set[Long], Set[Long], Int) => Boolean =
      (a : Set[Long], b : Set[Long], bits : Int) => {
      val longBits = implicitly[BoundedBits[Long]].bits
      val bits_ = if(bits == Long.MinValue) 1 else (bits.abs % longBits) + 1
      def bound(x : Long) = {
        val max = NBitLong.signContract(bits_ - 1, -1l)
        val min = NBitLong.signExtend(bits_, (1l << bits_ - 1))
        if(x == max || x == min) x else if(x >= 0) x % (max + 1) else x % (min - 1)
      }
      val aBounded = a.map(bound(_))
      val bBounded = b.map(bound(_))
      val a_ = (IntLikeSet[Long, NBitLong](bits_) /: aBounded)((acc, x) => acc + NBitLong(bits_, x))
      val b_ = (IntLikeSet[Long, NBitLong](bits_) /: bBounded)((acc, x) => acc + NBitLong(bits_, x))
      val ref = cartesianProduct(aBounded, bBounded).map((x) => NBitLong.signContract(bits_, normalop(bits_, x._1, x._2)))
      val us = intlikeop(a_, b_)
      val castIT = implicitly[Castable[(Int, Long), NBitLong]]
      val ref_ = ref.map((x : Long) => castIT((bits_, x)))
      val res = us == ref_
      if(!res) println("inputa_: " + a_ + "inputb_: " + b_ + ", bits: " + bits_ + ", us: " + us + ", ref: " + ref_ + ", result: " + (us == ref_).toString)
      res
      }
}