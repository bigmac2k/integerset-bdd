package cc.sven.tlike

import scala.collection.SetLike
import cc.sven.bdd._
import cc.sven.intset._
import cc.sven.bounded._
import scala.collection.JavaConverters._
import java.lang.AssertionError
import java.lang.AssertionError

class BitWidthException(widthA : Int, widthB : Int) extends Exception {
  override def toString = widthB.toString + " does not match required " + widthA.toString
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
  def remove(ele : T) = this - ele
  def +(ele : T) = {
    checkBitWidth(this, ele)
    val eleI = castTI(ele)._2
    new IntLikeSet[I, T](bits, set + eleI)
  }
  def add(ele : T) = this + ele
  def contains(ele : T) = {
    checkBitWidth(this, ele)
    val eleI = castTI(ele)
    set contains eleI._2
  }
  def unary_! = new IntLikeSet[I, T](bits, !set)
  def invert = !this
  def ite(t : IntLikeSet[I, T], e : IntLikeSet[I, T]) : IntLikeSet[I, T] = {
    checkBitWidth(this, t)
    checkBitWidth(t, e)
    new IntLikeSet(bits, set.ite(t.set, e.set))
  }
  def intersect(that : IntLikeSet[I, T]) = {
    checkBitWidth(this, that)
    new IntLikeSet[I, T](bits, set intersect that.set)
  }
  def &(that : IntLikeSet[I, T]) = this intersect that
  def union(that : IntLikeSet[I, T]) = {
    checkBitWidth(this, that)
    new IntLikeSet[I, T](bits, set union that.set)
  }
  def |(that : IntLikeSet[I, T]) = this union that
  def max = castIT((bits, set.max))
  def min = castIT((bits, set.min))
  def sizeBigInt = {
    import scala.math.BigInt.int2bigInt
    set.cbdd.truePaths.map((x) => 1l << (boundedBits.bits - x.length)).sum
  }
  override def size : Int = {
    val bint = sizeBigInt
    if(bint > Integer.MAX_VALUE) throw new IllegalArgumentException("size does not fit into an Int")
    bint.intValue
  }
  //XXX todo[SCM]: Check!
  def sizeGreaterThan(value : BigInt) : Boolean = {
    import scala.math.BigInt._
    var size : BigInt = 0
    for(p <- set.cbdd.truePaths) {
      size += 2 pow (bits - p.length)
      if(size > value) return true
    }
    return false
  }
  def sizeGreaterThan(value : Int) : Boolean = sizeGreaterThan(value : BigInt)
  def randomElement() = castIT((bits, set.randomElement()))
  def subsetOf(that : IntLikeSet[I, T]) = {
    checkBitWidth(this, that)
    set subsetOf that.set
  }
  override def isEmpty = set.isEmpty
  override def nonEmpty = set.nonEmpty
  private def getBWCBDD = set.cbdd.partialEval(List.fill(boundedBits.bits - bits)(false)) match {
    case Some(x) => x
    case _ => {
      assert(false, "Integrity failure")
      ???
    }
  }
  private def fromBWCBDD(bdd : CBDD) = new IntLikeSet[I, T](bits, new IntSet[I](CBDD(List.fill(boundedBits.bits - bits)(false), False, False, bdd)))
  def isFull = getBWCBDD match {
    case True => true
    case _ => false
  }
  def plus(that : IntLikeSet[I, T]) : IntLikeSet[I, T] = {
    val (norm, ov) = this.plusWithCarry(that)
    norm | ov
  }
  def plusWithCarry(that : IntLikeSet[I, T]) = {
    checkBitWidth(this, that)
    val (norm, ov) = CBDD.plus(getBWCBDD, that.getBWCBDD, bits)
    (fromBWCBDD(norm), fromBWCBDD(ov))
  }
  def negate = fromBWCBDD(CBDD.negate(bits, getBWCBDD))
  def bAnd(that : IntLikeSet[I, T]) = {
    checkBitWidth(this, that)
    new IntLikeSet[I, T](bits, set bAnd that.set)
  }
  def bOr(that : IntLikeSet[I, T]) = {
    checkBitWidth(this, that)
    new IntLikeSet[I, T](bits, set bOr that.set)
  }
  def bXOr(that : IntLikeSet[I, T]) = {
    checkBitWidth(this, that)
    new IntLikeSet[I, T](bits, set bXOr that.set)
  }
  def checkIntegrity() {
    def helper(cbdd : CBDD, depth : Int) : Boolean = cbdd match {
      case _ if depth == 0 => true
      case Node(False, uset) => helper(uset, depth - 1)
      case _ => false
    }
    assert(helper(set.cbdd, boundedBits.bits - bits), "Integrity failure in explicit check")
  }
  def iterator() = new IntLikeSetIterator(this)(castIT)
  def java = this.asJava
  def bitExtract(from : Int, to : Int) : IntLikeSet[I, T] = {
    require(from >= to && to >= 0)
    val toDrop = bits - from - 1
    val toTake = from - to + 1
    val toFill = bits - toTake
    def takeFkt(cbdd : CBDD) : CBDD = cbdd.take(toTake)
    def reduceFkt(a : CBDD, b : CBDD) = a || b
    val nCBDD = CBDD(List.fill(toFill)(false), False, False, set.cbdd.drop(toDrop, False, takeFkt, reduceFkt))
    val iSet = new IntSet[I](nCBDD)
    new IntLikeSet[I, T](toTake, iSet)
  }
  def signExtend(from : Int, to : Int) : IntLikeSet[I, T] = {
    require(from >= to && to >= 0)
    val nBits = bits max (from + 1)
    assert(nBits <= boundedBits.bits, "Inner type not large enough")
    val mask = CBDD(List.fill(boundedBits.bits - 1 - from)(false) ++ List.fill(from - to + 1)(true) ++ List.fill(to)(false))
    val (neg, pos) = getBWCBDD match {
      case True => (True, True)
      case False => (False, False)
      case Node(set, uset) => (Node(set, False), Node(False, uset))
    }
    val negBDD = fromBWCBDD(neg).set.cbdd
    val posBDD = fromBWCBDD(pos).set.cbdd
    //println(new IntSet[I](mask), new IntSet[I](negBDD), new IntSet[I](posBDD))
    val nBDD = negBDD match {
      case False => posBDD
      case _ => CBDD.bOr(negBDD, mask) || posBDD
    }
    new IntLikeSet[I, T](nBits, new IntSet[I](nBDD))
  }
  def zeroFill(from : Int, to : Int) : IntLikeSet[I, T] = {
    require(from >= to && to >= 0)
    val nBits = bits max (from + 1)
    val mask = CBDD(List.fill(boundedBits.bits - 1 - from)(true) ++ List.fill(from - to + 1)(false) ++ List.fill(to)(true))
    val nBDD = CBDD.bAnd(set.cbdd, mask)
    new IntLikeSet[I, T](nBits, new IntSet[I](nBDD))
  }
}
object IntLikeSet {
  def apply[I, T](bits : Int)(implicit int : Integral[I], bounded : Bounded[I], boundedBits : BoundedBits[I], dboundedBits : DynBoundedBits[T], castTI : Castable[T, (Int, I)], castIT : Castable[(Int, I), T]) : IntLikeSet[I, T] = new IntLikeSet(bits, IntSet[I]()(int, bounded, boundedBits))
  def apply[I, T](bits : Int, set : Set[T])(implicit int : Integral[I], bounded : Bounded[I], boundedBits : BoundedBits[I], dboundedBits : DynBoundedBits[T], castTI : Castable[T, (Int, I)], castIT : Castable[(Int, I), T]) : IntLikeSet[I, T] = new IntLikeSet(bits, IntSet[I](set.map(castTI(_)._2))(int, bounded, boundedBits))
  def apply[I, T](set : Set[T])(implicit int : Integral[I], bounded : Bounded[I], boundedBits : BoundedBits[I], tboundedBits : BoundedBits[T], dboundedBits : DynBoundedBits[T], castTI : Castable[T, (Int, I)], castIT : Castable[(Int, I), T]) : IntLikeSet[I, T] = apply(tboundedBits.bits, set)
  def applyJLong[T](bits : Int)(implicit dboundedBits : DynBoundedBits[T], castTI : Castable[T, cc.sven.misc.Pair[Integer, java.lang.Long]], castIT : Castable[cc.sven.misc.Pair[Integer, java.lang.Long], T]) : IntLikeSet[java.lang.Long, T] = {
    import cc.sven.misc.Pair
    implicit val castITT = new Castable[(Int, java.lang.Long), T] {
      def apply(p : (Int, java.lang.Long)) = castIT(Pair(p._1, p._2))
    }
    implicit val castTIT = new Castable[T, (Int, java.lang.Long)] {
      def apply(t : T) : (Int, java.lang.Long) = {
        val temp = castTI(t)
        (temp._1, temp._2)
      }
    }
    apply[java.lang.Long, T](bits)(cc.sven.integral.Implicits.jLongIsIntegral, cc.sven.bounded.Bounded.jLongIsBounded, cc.sven.bounded.BoundedBits.jLongIsBoundedBits, dboundedBits, castTIT, castITT)
  }
}

object Implicits {
  implicit def intLikeSetIsDynBounded[I, T] : DynBoundedBits[IntLikeSet[I, T]] = new DynBoundedBits[IntLikeSet[I, T]] {
    def dBits(set : IntLikeSet[I, T]) : Int = set.bits
  }
}

class IntLikeSetIterator[I, T](ilsi : IntLikeSet[I, T])(implicit castIT : Castable[(Int, I), T]) extends Iterator[T] {
  val ilsiIter = ilsi.set.iterator()
  def hasNext() = ilsiIter.hasNext()
  def next() = castIT((ilsi.bits, ilsiIter.next()))
}