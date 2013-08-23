package cc.sven.intset

import cc.sven.bdd._
import cc.sven.bounded._
import scala.collection.SetLike
import scala.collection.IterableLike
import scala.collection.JavaConverters._
import cc.sven.bounded.BoundedBits
import cc.sven.bounded.Bounded
import cc.sven.intset.IntegerIntegral._
import scala.math.BigInt.int2bigInt

//XXX Think about having the first bid (msb) have a flipped interpretation for signed values

//XXX rework ivals such that they can be empty (add bottom element?)
sealed abstract trait Ival[+T] extends Iterable[T] with IterableLike[T, Iterable[T]]
object Ival {
  def apply[T](lo : T, hi : T)(implicit int : Integral[T]) : Ival[T] = {
    import int.{ mkNumericOps, mkOrderingOps}
    if(lo > hi) EmptyIval else FilledIval(lo, hi)
  }
}
case object EmptyIval extends Ival[Nothing] {
  def iterator() = EmptyIterator
  override def toString() = "[]"
}
object EmptyIterator extends Iterator[Nothing] {
  def hasNext = false
  def next() = throw new NoSuchElementException("next on empty iterator")
}
final case class FilledIval[T](val lo : T, val hi : T)(implicit int : Integral[T]) extends Ival[T] {
  import int.{ mkNumericOps, mkOrderingOps}
  require(lo <= hi)
  def iterator = new IvalIterator(this)
  override def toString() = "[" + lo.toString + " .. " + hi.toString + "]"
}
class IvalIterator[T](ival : Ival[T])(implicit int : Integral[T]) extends Iterator[T] {
  import int.{mkNumericOps, mkOrderingOps}
  private var currIval = ival
  def hasNext() = currIval match {
    case EmptyIval => false
    case FilledIval(lo, hi) => true
  }
  def next() = currIval match {
    case FilledIval(lo, hi) if(lo == hi) => {
      currIval = EmptyIval
      lo
    }
    case FilledIval(lo, hi) => {
      currIval = FilledIval(lo + int.one, hi)
      lo
    }
  }
}

class IntSet[T](val cbdd : CBDD)(implicit int : Integral[T], bounded : Bounded[T], boundedBits : BoundedBits[T]) extends Set[T] with SetLike[T, IntSet[T]] {
  //enumeration
  //interval
  //strided ival
  /*todo:
   * equals, canEqual
   * diff (--)
   */
  def unary_! = new IntSet[T](!cbdd)
  def invert = !this
  def ite(t : IntSet[T], e : IntSet[T]) = new IntSet[T](cbdd.ite(t.cbdd, e.cbdd))
  def partialEval(bs : List[Boolean]) = new IntSet[T](cbdd.partialEval(bs))
  def +(elem : T) : IntSet[T] = new IntSet[T](cbdd || IntSet.apply[T](elem)(int, bounded, boundedBits).cbdd)
  def add(elem : T) = this + elem
  def -(elem : T) : IntSet[T] = new IntSet[T](cbdd && !IntSet.apply[T](elem)(int, bounded, boundedBits).cbdd)
  def remove(elem : T) = this - elem
  def contains(elem : T) : Boolean = partialEval(IntSet.toBitVector(elem)).cbdd match {
    case True => true
    case False => false
  }
  def iterator() = new IntSetIterator(this)
  override def empty = new IntSet(False)
  def intersect(that : IntSet[T]) : IntSet[T] = new IntSet(cbdd && that.cbdd)
  def &(that : IntSet[T]) : IntSet[T] = this intersect that
  def union(that : IntSet[T]) : IntSet[T] = new IntSet(cbdd || that.cbdd)
  def |(that : IntSet[T]) : IntSet[T] = this union that
  def max = cbdd match {
    case False => throw new UnsupportedOperationException
    case True => IntSet.fromBitVector(List(false).padTo(boundedBits.bits - 1, true))(int, bounded, boundedBits)
    //set cannot be false because of canonical bdd, threfore set.trueMost != None
    case Node(set, False) => IntSet.fromBitVector(true :: set.trueMost.get.padTo(boundedBits.bits - 1, true))(int, bounded, boundedBits)
    case Node(_, uset) => IntSet.fromBitVector(false :: uset.trueMost.get.padTo(boundedBits.bits - 1, true))(int, bounded, boundedBits)
  }
  def min = cbdd match {
    case False => throw new UnsupportedOperationException
    case True => IntSet.fromBitVector(List(true).padTo(boundedBits.bits - 1, false))(int, bounded, boundedBits)
    case Node(False, uset) => IntSet.fromBitVector(false :: uset.falseMost.get.padTo(boundedBits.bits - 1, false))(int, bounded, boundedBits)
    case Node(set, _) => IntSet.fromBitVector(true :: set.falseMost.get.padTo(boundedBits.bits - 1, false))(int, bounded, boundedBits)
  }
  def sizeBigInt : BigInt = {
    import scala.math.BigInt._  
    (cbdd.truePaths.map((x) => 2 pow (boundedBits.bits - x.length))).sum
  }
  def subsetOf(that : IntSet[T]) : Boolean = this.cbdd doesImply that.cbdd 
  override def isEmpty : Boolean = this.cbdd match { case False => true; case _ => false }
  override def nonEmpty : Boolean = !this.isEmpty //perhaps not needed - see implementation
  def isFull : Boolean = this.cbdd match { case True => true; case _ => false }
}

class IntSetIterator[T](iset : IntSet[T])(implicit int : Integral[T], bounded : Bounded[T], boundedBits : BoundedBits[T]) extends Iterator[T] {
  val iter = new CBDDIterator(iset.cbdd, boundedBits.bits)
  def hasNext() = iter.hasNext()
  def next() = IntSet.fromBitVector(iter.next())(int, bounded, boundedBits)
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
  def apply[T](is : T*)(implicit int : Integral[T], bounded : Bounded[T], boundedBits : BoundedBits[T]) : IntSet[T] = (IntSet(Set[T]()) /: is){
    (acc, i) => acc | IntSet(Set(i))
  }
  def apply[T](i : T)(implicit int : Integral[T], bounded : Bounded[T], boundedBits : BoundedBits[T]) : IntSet[T] = new IntSet (CBDD(toBitVector(i)(int, boundedBits)))
  def apply[T](s : Set[T])(implicit int : Integral[T], bounded : Bounded[T], boundedBits : BoundedBits[T]) : IntSet[T] = new IntSet (((False : CBDD) /: s){
    (bdd, i) =>
      val ibdd = CBDD(toBitVector(i)(int, boundedBits))
      bdd || ibdd
  })
  def apply[T](s : java.util.Set[T])(implicit int : Integral[T], bounded : Bounded[T], boundedBits : BoundedBits[T]) : IntSet[T] = apply(s.asScala.toSet)
  def apply[T](ival : Ival[T])(implicit int : Integral[T], bounded : Bounded[T], boundedBits : BoundedBits[T]) : IntSet[T] = {
    import int.{mkNumericOps, mkOrderingOps}
    def smallerBV(fullLenBV : List[Boolean]) = CBDD(fullLenBV, False, True, True)
    def greaterBV(fullLenBV : List[Boolean]) = CBDD(fullLenBV, True, False, True)
    ival match {
      case EmptyIval => IntSet(Set[T]())
      case FilledIval(lo, hi) if(lo < int.zero) => {
        val greaterSet = apply(Ival(int.zero, hi))
        val smallerSet = new IntSet(greaterBV(toBitVector(lo)) && smallerBV(toBitVector(int.min(-int.one, hi))))(int, bounded, boundedBits)
        greaterSet union smallerSet
      }
      case FilledIval(lo, hi) => new IntSet(smallerBV(toBitVector(hi)) && greaterBV(toBitVector(lo)))
    }
  }
  /*def apply(i : Integer) : IntSet[Integer] = {
    val int = implicitly[Integral[Integer]]
    val bounded = implicitly[Bounded[Integer]]
    val boundedBits = implicitly[BoundedBits[Integer]]
    apply(i)(int, bounded, boundedBits)
  }*/
}