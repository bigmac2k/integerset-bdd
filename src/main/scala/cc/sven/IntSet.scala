package cc.sven.intset

import cc.sven.bdd._
import cc.sven.bounded._

//Decided not to use case class to use inheritance (Ival <- StridedIval)
class Ival[T](val lo: T, val hi: T)(implicit int: Integral[T]) extends Set[T] {
  import int.{ mkNumericOps, mkOrderingOps }
  require(lo <= hi)
  override def toString = "[" + lo.toString + " .. " + hi.toString + "]"
  def +(elem: T) = if (elem < lo) Ival(elem, hi) else if (elem > hi) Ival(lo, elem) else
    this
  def -(elem: T) = if (elem equiv lo) Ival(lo + int.one, hi) else if (elem equiv hi) Ival(lo, hi - int.one) else
    this
  def contains(elem: T) = elem >= lo && elem <= hi
  def iterator() = new IvalIterator(this)
}
class IvalIterator[T](ival: Ival[T])(implicit int: Integral[T]) extends Iterator[T] {
  import int.{ mkNumericOps, mkOrderingOps }
  private var curr = ival.lo
  def hasNext() = curr <= ival.hi
  def next() = {
    assert(hasNext())
    val currNow = curr
    curr = curr + int.one
    currNow
  }
}
object Ival {
  def apply[T](lo: T, hi: T)(implicit int: Integral[T]): Ival[T] = new Ival(lo, hi)
  def unapply[T](ival: Ival[T]) = Some(ival.lo, ival.hi)
}
/*class StridedIval[T](override val lo : T, override val hi : T, val stride : T)(implicit int : Integral[T]) extends Ival[T](lo, hi)(int) {
  import int.{mkNumericOps, mkOrderingOps}
  private[this] val next = lo + stride
  require(lo < next && next <= hi)
  override def toString = "[" + lo.toString + ", " + int.plus(lo, stride).toString + " .. " + hi.toString + "]"
}

//XXX implement Iterator, override + - contains iterator
object StridedIval {
  def apply[T](lo : T, next : T, hi : T)(implicit int : Integral[T]) : StridedIval[T] = {
    import int.{mkNumericOps, mkOrderingOps}
    val stride = next - lo
    new StridedIval(lo,  hi, stride)
  }
  def unapply[T](ival : StridedIval[T]) = {
  }
}*/

/*
class IntSet[T](val cbdd : CBDD)(implicit int : Integral[T], boundedBits : BoundedBits[T]) extends BDDLike[IntSet[T]] with Set[T] {
  //enumeration
  //interval
  //strided ival
  val BDDTrue = new IntSet[T](True)
  val BDDFalse = new IntSet[T](False)
  def unary_! = new IntSet[T](!cbdd)
  def ite(t : IntSet[T], e : IntSet[T]) = new IntSet[T](cbdd.ite(t.cbdd, e.cbdd))
  def partialEval(bs : List[Boolean]) = new IntSet[T](cbdd.partialEval(bs))
  def +(elem : T) : IntSet[T] = new IntSet[T](cbdd || IntSet.apply[T](elem)(int, boundedBits).cbdd)
  def -(elem : T) : IntSet[T] = new IntSet[T](cbdd && IntSet.apply[T](elem)(int, boundedBits).cbdd)
  def contains(elem : T) : Boolean = partialEval(IntSet.toBitVector(elem)).cbdd match {
    case True => true
    case False => false
  }
}

object IntSet {
  def toBitVector[T](i: T)(implicit int: Integral[T], boundedBits: BoundedBits[T]): List[Boolean] = {
    import int.{ mkNumericOps, mkOrderingOps }
      def helper(rest: T, bitvector: List[Boolean], c: Int): List[Boolean] = {
        if (rest equiv int.zero) List.fill(c)(false) ++ bitvector else helper(rest / int.fromInt(2), (rest % int.fromInt(2) != int.zero) :: bitvector, c - 1)
      }
    helper(i, List.empty, boundedBits.bits)
  }
  def apply[T](i : T)(implicit int : Integral[T], boundedBits : BoundedBits[T]) : IntSet[T] = new IntSet (CBDD(toBitVector(i)(int, boundedBits)))
  def apply[T](s : Set[T])(implicit int : Integral[T], boundedBits : BoundedBits[T]) : IntSet[T] = new IntSet (((False : CBDD) /: s){
    (bdd, i) =>
      val ibdd = CBDD(toBitVector(i)(int, boundedBits))
      bdd || ibdd
  })
}*/