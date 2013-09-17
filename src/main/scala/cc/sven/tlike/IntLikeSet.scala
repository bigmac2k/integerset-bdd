package cc.sven.tlike

import scala.collection.SetLike
import cc.sven.intset._
import cc.sven.bounded._
import scala.collection.JavaConverters._

class BitWidthException(widthA : Int, widthB : Int) extends Exception {
  override def toString = widthA.toString + " does not match required " + widthB.toString
}
/* a set for types that wrap ints with a fixed bit width - needs type with fixed bit width that is greater than included */
class IntLikeSet[I, T](val bits : Int, val set : IntSet[I])
                      (implicit int : Integral[I], bounded : Bounded[I], boundedBits : BoundedBits[I], dboundedBits : DynBoundedBits[T], castTI : Castable[T, Pair[Int, I]], castIT : Castable[Pair[Int, I], T])
                      extends Set[T] with SetLike[T, IntLikeSet[I, T]] {  
  import Implicits._
  private def checkBitWidth[A, B](a : A, b : B)(implicit ab : DynBoundedBits[A], bb : DynBoundedBits[B]) {
    if(ab.dBits(a) != bb.dBits(b)) throw new BitWidthException(ab.dBits(a), bb.dBits(b))
  }
  override def empty : IntLikeSet[I, T] = new IntLikeSet[I, T](bits, IntSet[I]()(int, bounded, boundedBits))
  def -(ele : T) = {
    checkBitWidth(this, ele)
    val eleI = castTI(ele)._2
    new IntLikeSet[I, T](bits, set - eleI)
  }
  def +(ele : T) = {
    checkBitWidth(this, ele)
    val eleI = castTI(ele)._2
    new IntLikeSet[I, T](bits, set + eleI)
  }
  def contains(ele : T) = {
    checkBitWidth(this, ele)
    val eleI = castTI(ele)
    set contains eleI._2
  }
  def iterator() = new IntLikeSetIterator(this)
  def java = this.asJava
}
object IntLikeSet {
  def apply[I, T](bits : Int)(implicit int : Integral[I], bounded : Bounded[I], boundedBits : BoundedBits[I], dboundedBits : DynBoundedBits[T], castTI : Castable[T, (Int, I)], castIT : Castable[(Int, I), T]) = new IntLikeSet(bits, IntSet[I]()(int, bounded, boundedBits))
}

object Implicits {
  implicit def intLikeSetIsDynBounded[I, T] : DynBoundedBits[IntLikeSet[I, T]] = new DynBoundedBits[IntLikeSet[I, T]] {
    def dBits(set : IntLikeSet[I, T]) : Int = set.bits
  }
}

class IntLikeSetIterator[I, T](ilsi : IntLikeSet[I, T])(implicit castIT : Castable[(Int, I), T]) extends Iterator[T] {
  val ilsiIter = ilsi.set.iterator
  def hasNext() = ilsiIter.hasNext
  def next() = castIT(ilsi.bits, ilsiIter.next)
}