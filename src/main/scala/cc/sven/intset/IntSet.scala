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
sealed abstract trait Ival[+T] extends Iterable[T] with IterableLike[T, Iterable[T]] {
  def lo : T
  def hi : T
}
object Ival {
  def apply[T](lo: T, hi: T)(implicit int: Integral[T]): Ival[T] = {
    import int.{ mkNumericOps, mkOrderingOps }
    if (lo > hi) EmptyIval else FilledIval(lo, hi)
  }
}
case object EmptyIval extends Ival[Nothing] {
  def lo = throw new IllegalArgumentException("lo on empty interval")
  def hi = throw new IllegalArgumentException("hi on empty interval")
  def iterator() = EmptyIterator
  override def toString() = "[]"
}
object EmptyIterator extends Iterator[Nothing] {
  def hasNext = false
  def next() = throw new NoSuchElementException("next on empty iterator")
}
final case class FilledIval[T](val lo: T, val hi: T)(implicit int: Integral[T]) extends Ival[T] {
  import int.{ mkNumericOps, mkOrderingOps }
  require(lo <= hi)
  def iterator = new IvalIterator(this)
  override def toString() = "[" + lo.toString + " .. " + hi.toString + "]"
}
class IvalIterator[T](ival: Ival[T])(implicit int: Integral[T]) extends Iterator[T] {
  import int.{ mkNumericOps, mkOrderingOps }
  private var currIval = ival
  def hasNext() = currIval match {
    case EmptyIval          => false
    case FilledIval(lo, hi) => true
  }
  def next() = currIval match {
    case FilledIval(lo, hi) if (lo == hi) => {
      currIval = EmptyIval
      lo
    }
    case FilledIval(lo, hi) => {
      currIval = FilledIval(lo + int.one, hi)
      lo
    }
  }
}

class IntSet[T](val cbdd: CBDD)(implicit int: Integral[T], bounded: Bounded[T], boundedBits: BoundedBits[T]) extends Set[T] with SetLike[T, IntSet[T]] {
  //enumeration
  //interval
  //strided ival
  /*todo:
   * equals, canEqual
   * diff (--)
   */
  def unary_! = new IntSet[T](!cbdd)
  def invert = !this
  def ite(t: IntSet[T], e: IntSet[T]) = new IntSet[T](cbdd.ite(t.cbdd, e.cbdd))
  def partialEval(bs: List[Boolean]) = new IntSet[T](cbdd.partialEval(bs))
  def +(elem: T): IntSet[T] = new IntSet[T](cbdd || IntSet.apply[T](elem)(int, bounded, boundedBits).cbdd)
  def add(elem: T) = this + elem
  def -(elem: T): IntSet[T] = new IntSet[T](cbdd && !IntSet.apply[T](elem)(int, bounded, boundedBits).cbdd)
  def remove(elem: T) = this - elem
  def contains(elem: T): Boolean = partialEval(IntSet.toBitVector(elem)).cbdd match {
    case True  => true
    case False => false
  }
  def iterator() = new IntSetIterator(this)
  override def empty = new IntSet(False)
  def intersect(that: IntSet[T]): IntSet[T] = new IntSet(cbdd && that.cbdd)
  def &(that: IntSet[T]): IntSet[T] = this intersect that
  def union(that: IntSet[T]): IntSet[T] = new IntSet(cbdd || that.cbdd)
  def |(that: IntSet[T]): IntSet[T] = this union that
  def max = cbdd match {
    case False            => throw new UnsupportedOperationException
    case True             => IntSet.fromBitVector(List(false).padTo(boundedBits.bits - 1, true))(int, bounded, boundedBits)
    //set cannot be false because of canonical bdd, threfore set.trueMost != None
    case Node(set, False) => IntSet.fromBitVector(true :: set.trueMost.get.padTo(boundedBits.bits - 1, true))(int, bounded, boundedBits)
    case Node(_, uset)    => IntSet.fromBitVector(false :: uset.trueMost.get.padTo(boundedBits.bits - 1, true))(int, bounded, boundedBits)
  }
  def min = cbdd match {
    case False             => throw new UnsupportedOperationException
    case True              => IntSet.fromBitVector(List(true).padTo(boundedBits.bits - 1, false))(int, bounded, boundedBits)
    case Node(False, uset) => IntSet.fromBitVector(false :: uset.falseMost.get.padTo(boundedBits.bits - 1, false))(int, bounded, boundedBits)
    case Node(set, _)      => IntSet.fromBitVector(true :: set.falseMost.get.padTo(boundedBits.bits - 1, false))(int, bounded, boundedBits)
  }
  def sizeBigInt: BigInt = {
    import scala.math.BigInt._
    (cbdd.truePaths.map((x) => 2 pow (boundedBits.bits - x.length))).sum
  }
  def randomElement() = {
    val path = this.cbdd.randomTruePath()
    val path_ = path ++ List.fill(boundedBits.bits - path.length)(scala.util.Random.nextBoolean())
    IntSet.fromBitVector(path_)(int, bounded, boundedBits)
  }
  def subsetOf(that: IntSet[T]): Boolean = this.cbdd doesImply that.cbdd
  override def isEmpty: Boolean = this.cbdd match { case False => true; case _ => false }
  override def nonEmpty: Boolean = !this.isEmpty //perhaps not needed - see implementation
  def isFull: Boolean = this.cbdd match { case True => true; case _ => false }
  /* XXX todo
   * cartesian product operation (+, -, bitwise, mult, div)
   * idea for optimization:
   *   since with msb->lsb ordering, the bdd is essentially a set of intervals where the set is held small
   *   by precise ivals, we could find more precise intervals by "communicating" breadth first search.
   *   Communicating, because we could say: recurse up to $n$ times.
   *   this could be implemented as a widening operator
   */
  private type CBDDTuple = (CBDD, CBDD)
  private def addMerge(ff : CBDDTuple, ft : CBDDTuple, tf : CBDDTuple, tt : CBDDTuple) : CBDDTuple = {
    def union3(a : CBDD, b : CBDD, c : CBDD) : CBDD = List(a, b, c).distinct.reduce(_ || _)
    val trueOVNot = union3(tf._1, ft._1, ff._2)
    val falseOVNot = ff._1
    val trueOV = tt._2
    val falseOV = union3(tt._1, tf._2, ft._2)
    (Node(trueOVNot, falseOVNot), Node(trueOV, falseOV))
  }
  private def plus(op1 : CBDD, op2 : CBDD, depth : Int) : CBDDTuple = (op1, op2) match {
    case (False, _) => (False, False)
    case (x, False) => plus(False, x, depth)
    case (True, True) => {
      val ov = if(depth == 0) False else !CBDD(List.fill(depth)(true))
      (True, ov)
    }
    case (Node(set, uset), True) => {
      val tt = plus(set, True, depth - 1)
      val tf = tt
      val ff = plus(uset, True, depth - 1)
      val ft = ff
      addMerge(ff, ft, tf, tt)
    }
    case (True, x) => plus(x, True, depth)
    case (Node(set1, uset1), Node(set2, uset2)) => {
      /*optimization:
       * op1 == op2 -> op1 << 1 XXX
       * set1 == uset1 -> tt = ft, tf = ff
       * set2 == uset2 -> tt = tf, ft = ff
       */
      val tt = plus(set1, set2, depth - 1)
      val ft = if(set1 == uset1) tt else plus(uset1, set2, depth - 1)
      val ff = if(set2 == uset2) ft else plus(uset1, uset2, depth - 1)
      val tf = if(set1 == uset1) ff else if(set2 == uset2) tt else plus(set1, uset2, depth - 1)
      addMerge(ff, ft, tf, tt)
    }
  }
  def plus(that : IntSet[T]) : IntSet[T] = {
    val res = plus(this.cbdd, that.cbdd, boundedBits.bits)
    new IntSet(res._1 || res._2)
  }
  def plusWithCarry(that : IntSet[T]) : (IntSet[T], IntSet[T]) = {
    val res = plus(this.cbdd, that.cbdd, boundedBits.bits)
    (new IntSet(res._1), new IntSet(res._2))
  }
  //XXX this function is a stub
  def mult(that : IntSet[T]) : IntSet[T] = {
    import int.{mkNumericOps, mkOrderingOps}
    val thisIval = Ival(this.min, this.max)
    val thatIval = Ival(that.min, that.max)
    null
  }
  private def bitwiseOpHelper(op : (Boolean, Boolean) => Boolean)(trueFalseTuples : List[((CBDD, Boolean), (CBDD, Boolean))]) : CBDD = {
    val trueFalse = trueFalseTuples.distinct.partition((t) => op(t._1._2, t._2._2))
    val nset = ((False : CBDD) /: trueFalse._1)((acc, t) => acc || bitwiseOp(op)(t._1._1, t._2._1))
    val nuset = ((False : CBDD) /: trueFalse._2)((acc, t) => acc || bitwiseOp(op)(t._1._1, t._2._1))
    Node(nset, nuset)
  }
  //XXX should probably be specialized
  private def bitwiseOp(op : (Boolean, Boolean) => Boolean)(op1 : CBDD, op2 : CBDD) : CBDD = (op1, op2) match {
    case (False, _) => False
    case (x, False) => bitwiseOp(op)(False, x)
    case (True, True) => True
    case (Node(set, uset), True) => {
      val trueFalseTuples = List(((set, true), (True, true)), ((set, true), (True, false)), ((uset, false), (True, true)), ((uset, false), (True, false)))
      bitwiseOpHelper(op)(trueFalseTuples)
    }
    case (True, Node(set, uset)) => {//list this case explicitly to not force op to be commutative
      val trueFalseTuples = List(((True, true), (set, true)), ((True, true), (uset, false)), ((True, false), (set, true)), ((True, false), (uset, false)))
      bitwiseOpHelper(op)(trueFalseTuples)
    }
    case (Node(set1, uset1), Node(set2, uset2)) => {
      val trueFalseTuples = List(((set1, true), (set2, true)), ((uset1, false), (set2, true)), ((set1, true), (uset2, false)), ((uset1, false), (uset2, false)))
      /*val trueFalseTuples = for{
        n1 <- List((set1, true), (uset1, false))
        n2 <- List((set2, true), (uset2, false))
      } yield (n1, n2)*/
      bitwiseOpHelper(op)(trueFalseTuples)
    }
  }
  def bAnd(that : IntSet[T]) : IntSet[T] = new IntSet(bitwiseOp(_ && _)(this.cbdd, that.cbdd))
  def bOr(that : IntSet[T]) : IntSet[T] = new IntSet(bitwiseOp(_ || _)(this.cbdd, that.cbdd))
  def bXOr(that : IntSet[T]) : IntSet[T] = new IntSet(bitwiseOp(_ != _)(this.cbdd, that.cbdd))
}

class IntSetIterator[T](iset: IntSet[T])(implicit int: Integral[T], bounded: Bounded[T], boundedBits: BoundedBits[T]) extends Iterator[T] {
  val iter = new CBDDIterator(iset.cbdd, boundedBits.bits)
  def hasNext() = iter.hasNext()
  def next() = IntSet.fromBitVector(iter.next())(int, bounded, boundedBits)
}

object IntSet {
  def toBitVector[T](i: T)(implicit int: Integral[T], boundedBits: BoundedBits[T]): List[Boolean] = {
    import int.{ mkNumericOps, mkOrderingOps }
    val extendSign = i < int.zero
      def helper(rest: T, bitvector: List[Boolean], c: Int, carry: Boolean): List[Boolean] = {
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
  def fromBitVector[T](bs: List[Boolean])(implicit int: Integral[T], bounded: Bounded[T], boundedBits: BoundedBits[T]): T = {
    import int.{ mkNumericOps, mkOrderingOps }
    val bslen = bs.length
    require(bslen <= boundedBits.bits)
    val isNegative = bounded.minBound < int.zero && (bs match {
      case Nil       => false
      case sbit :: _ => sbit
    })
    val zipped = bs.zip(List.iterate(int.one, bslen)(_ * int.fromInt(2)).reverse)
    val positive = (int.zero /: zipped)((acc: T, tuple: (Boolean, T)) => if (tuple._1 != isNegative) acc + tuple._2 else acc)
    if (isNegative) -(positive + int.one) else positive
  }
  /* The following 3 apply methods are there for java interop.
   * one T* method would generate code that is not very useful in java
   * but we want at least apply(1) and apply() for java
   */
  def apply[T]()(implicit int: Integral[T], bounded: Bounded[T], boundedBits: BoundedBits[T]): IntSet[T] = IntSet(Set[T]())
  def apply[T](i1 : T, i2 : T, is: T*)(implicit int: Integral[T], bounded: Bounded[T], boundedBits: BoundedBits[T]): IntSet[T] = (IntSet(Set[T]()) /: (i1 :: i2 :: is.toList)) {
    (acc, i) => acc | IntSet(Set(i))
  }
  def apply[T](i: T)(implicit int: Integral[T], bounded: Bounded[T], boundedBits: BoundedBits[T]): IntSet[T] = new IntSet(CBDD(toBitVector(i)(int, boundedBits)))
  def apply[T](s: Set[T])(implicit int: Integral[T], bounded: Bounded[T], boundedBits: BoundedBits[T]): IntSet[T] = new IntSet(((False: CBDD) /: s) {
    (bdd, i) =>
      val ibdd = CBDD(toBitVector(i)(int, boundedBits))
      bdd || ibdd
  })
  def apply[T](s: java.util.Set[T])(implicit int: Integral[T], bounded: Bounded[T], boundedBits: BoundedBits[T]): IntSet[T] = apply(s.asScala.toSet)
  def apply[T](ival: Ival[T])(implicit int: Integral[T], bounded: Bounded[T], boundedBits: BoundedBits[T]): IntSet[T] = {
    import int.{ mkNumericOps, mkOrderingOps }
      def smallerBV(fullLenBV: List[Boolean]) = CBDD(fullLenBV, False, True, True)
      def greaterBV(fullLenBV: List[Boolean]) = CBDD(fullLenBV, True, False, True)
    ival match {
      case EmptyIval => IntSet(Set[T]())
      case FilledIval(lo, hi) if (lo < int.zero) => {
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