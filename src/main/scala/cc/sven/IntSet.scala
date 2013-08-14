package cc.sven.intset

import cc.sven.bdd._
import cc.sven.bounded._
import scala.collection.SetLike

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


class IntSet[T](val cbdd : CBDD)(implicit int : Integral[T], bounded : Bounded[T], boundedBits : BoundedBits[T]) /*extends BDDLike[IntSet[T]]*/ extends Set[T] with SetLike[T, IntSet[T]] {
  //enumeration
  //interval
  //strided ival
  //val BDDTrue = new IntSet[T](True)
  //val BDDFalse = new IntSet[T](False)
  def unary_! = new IntSet[T](!cbdd)
  def ite(t : IntSet[T], e : IntSet[T]) = new IntSet[T](cbdd.ite(t.cbdd, e.cbdd))
  def partialEval(bs : List[Boolean]) = new IntSet[T](cbdd.partialEval(bs))
  def +(elem : T) : IntSet[T] = new IntSet[T](cbdd || IntSet.apply[T](elem)(int, bounded, boundedBits).cbdd)
  def -(elem : T) : IntSet[T] = new IntSet[T](cbdd && !IntSet.apply[T](elem)(int, bounded, boundedBits).cbdd)
  def contains(elem : T) : Boolean = partialEval(IntSet.toBitVector(elem)).cbdd match {
    case True => true
    case False => false
  }
  def iterator() = new IntSetIterator(this)
  override def empty = new IntSet(False)
}

class IntSetIterator[T](iset : IntSet[T])(implicit int : Integral[T], bounded : Bounded[T], boundedBits : BoundedBits[T]) extends Iterator[T] {
  val iter = new CBDDIterator(iset.cbdd, boundedBits.bits)
  def hasNext() = iter.hasNext()
  def next() = IntSet.fromBitVector(iter.next())
}

object IntSet {
  def toBitVector[T](i: T)(implicit int: Integral[T], boundedBits: BoundedBits[T]): List[Boolean] = {
    import int.{ mkNumericOps, mkOrderingOps }
    val extendSign = i < int.zero
      def helper(rest: T, bitvector: List[Boolean], c: Int, carry : Boolean): List[Boolean] = {
        if (rest equiv int.zero) List.fill(c)(extendSign) ++ bitvector else {
          //half adder for the +1 in two's complement
          val bit = ((rest % int.fromInt(2) != int.zero) != extendSign)
          val bitWithCarry = bit != carry
          val nextCarry = bit && carry
          helper(rest / int.fromInt(2), bitWithCarry :: bitvector, c - 1, nextCarry)
        }
      }
    helper(i, List.empty, boundedBits.bits, extendSign)
  }
  def fromBitVector[T](bs : List[Boolean])(implicit int : Integral[T], bounded : Bounded[T], boundedBits : BoundedBits[T]) : T = {
    import int.{ mkNumericOps, mkOrderingOps}
    val bslen = bs.length
    require(bslen <= boundedBits.bits)
    val isNegative = bounded.minBound < int.zero && (bs match {
      case Nil => false
      case sbit :: _ => sbit
    })
    val zipped = bs.zip(List.iterate(int.one, bslen)(_ * int.fromInt(2)).reverse)
    val positive = (int.zero /: zipped)((acc : T, tuple : (Boolean, T)) => if(tuple._1 != isNegative) acc + tuple._2 else acc)
    if(isNegative) -(positive + int.one) else positive
  }
  def apply[T](i : T)(implicit int : Integral[T], bounded : Bounded[T], boundedBits : BoundedBits[T]) : IntSet[T] = new IntSet (CBDD(toBitVector(i)(int, boundedBits)))
  def apply[T](s : Set[T])(implicit int : Integral[T], bounded : Bounded[T], boundedBits : BoundedBits[T]) : IntSet[T] = new IntSet (((False : CBDD) /: s){
    (bdd, i) =>
      val ibdd = CBDD(toBitVector(i)(int, boundedBits))
      bdd || ibdd
  })
}