package cc.sven.constraint

import cc.sven.bounded._
import scala.collection.immutable.IntMap
import scala.collection.immutable.Set
import cc.sven.tlike._

trait Constrainable[T, S[_]] {
  def range(lo : T, hi : T) : S[T]
  def intersect(a : S[T], b : S[T]) : S[T]
  //def intersect[SS >: S[T]](a : SS, b : SS) : S[T]
  def isEmpty(a : S[T]) : Boolean
  def min(a : S[T]) : T
  def max(a : S[T]) : T
  def invert(a : S[T]) : S[T]
}

object Constraint {
  implicit def intLikeSetIsConstrainable[I, T]
    (implicit int : Integral[I], bounded : Bounded[I], boundedBits : BoundedBits[I], dboundedBits : DynBoundedBits[T], castTI : Castable[T, (Int, I)], castIT : Castable[(Int, I), T]) =
    new Constrainable[T, ({type x[a]=IntLikeSet[I, a]})#x] {
      def range(lo : T, hi : T) = IntLikeSet.range[I, T](lo, hi)
      def intersect(a : IntLikeSet[I, T], b : IntLikeSet[I, T]) = a.intersect(b)
      def isEmpty(a : IntLikeSet[I, T]) = a.isEmpty
      def min(a : IntLikeSet[I, T]) = a.min
      def max(a : IntLikeSet[I, T]) = a.max
      def invert(a : IntLikeSet[I, T]) = !a
  }
}

sealed trait Constraint {
  //simplifies to only LT, LTE, Equals, NEquals, And, Not (i.e. no GT, GTE, Or)
  private def simplify : Constraint = this match {
    case GT(left, right) => LT(right, left)
    case GTE(left, right) => LTE(right, left)
    case Or(left, right) => Not(And(Not(left), Not(right))).simplify
    case Not(Not(op)) => op.simplify
    case Not(Equals(left, right)) => NEquals(left, right)
    case Not(NEquals(left, right)) => Equals(left, right)
    case Not(LT(left : Int, right : Int)) => GTE(left, right).simplify
    case Not(LTE(left : Int, right : Int)) => GT(left, right).simplify
    case Not(GT(left : Int, right : Int)) => LTE(left, right)
    case Not(GTE(left : Int, right : Int)) => LT(left, right)
    case Not(Or(left, right)) => And(Not(left).simplify, Not(right).simplify)
    case And(left, right) => And(left.simplify, right.simplify)
    case x => x
  }
  def getVarIds : Set[Int] = {
    def helper(iSet : Set[Int], c : Constraint) : Set[Int] = c match {
      case LT(l, r) => iSet + l + r
      case GT(l, r) => iSet + l + r
      case LTE(l, r) => iSet + l + r
      case GTE(l, r) => iSet + l + r
      case Equals(l, r) => iSet + l + r
      case NEquals(l, r) => iSet + l + r
      case Not(op) => helper(iSet, op)
      case And(l, r) => helper(helper(iSet, r), l)
      case Or(l, r) => helper(helper(iSet, r), l)
    }
    helper(Set[Int](), this)
  }
  def solve[T, S[_]](table : IntMap[S[T]])(implicit dBounded : DynBounded[T], ord : Ordering[T], const : Constrainable[T, S]) : IntMap[S[T]] = {
    import ord.mkOrderingOps
    val varIds = getVarIds
    //under each variable id of this, there needs to be a non-empty set in table
    require(varIds.forall((x) => !const.isEmpty(table(x))))
    //Build a state including all valid values
    val allFull = (IntMap.empty[S[T]] /: varIds){
      (acc, id) =>
        val setVal = const.min(table(id))
        val min = dBounded.dMinBound(setVal)
        val max = dBounded.dMaxBound(setVal)
        acc + (id, const.range(min, max))
    }
    def buildAllValid(formula : Constraint) : IntMap[S[T]] = formula match {
      case LTE(left, right) => {
        val vleft = table(left)
        val vright = table(right)
        val minLeft = const.min(vleft)
        val maxRight = const.max(vright)
        println("maxRight: " + maxRight + ", dBounded.dMaxBound(minLeft): " + dBounded.dMaxBound(minLeft))
        val validLeft = const.range(dBounded.dMinBound(minLeft), maxRight min dBounded.dMaxBound(minLeft))
        val validRight = const.range(minLeft max dBounded.dMinBound(maxRight), dBounded.dMaxBound(maxRight))
        allFull + ((left, validLeft)) + ((right, validRight))
      }
      case LT(left, right) => {
        val vleft = table(left)
        val vright = table(right)
        val minLeft = const.min(vleft)
        val maxLeft = const.max(vleft)
        val minRight = const.min(vright)
        val maxRight = const.max(vright)
        val validLeft = if(maxRight > dBounded.dMaxBound(maxLeft))
          //empty
          const.invert(const.range(dBounded.dMinBound(maxLeft), dBounded.dMaxBound(maxLeft)))
        else
          const.invert(const.range(maxRight, dBounded.dMaxBound(maxLeft)))
        val validRight = if(maxLeft > dBounded.dMaxBound(minRight))
          //empty
          const.invert(const.range(dBounded.dMinBound(maxRight), dBounded.dMaxBound(maxRight)))
        else
          const.invert(const.range(dBounded.dMinBound(minRight), minLeft))
        allFull + ((left, validLeft)) + ((right, validRight))
      }
    }
    //stub - should intersect table per value
    buildAllValid(this)
  }
}
final case class LT(left : Int, right : Int) extends Constraint
final case class GT(left : Int, right : Int) extends Constraint
final case class LTE(left : Int, right : Int) extends Constraint
final case class GTE(left : Int, right : Int) extends Constraint
final case class Equals(left : Int, right : Int) extends Constraint
final case class NEquals(left : Int, right : Int) extends Constraint
final case class Not(op : Constraint) extends Constraint
final case class And(left : Constraint, right : Constraint) extends Constraint
final case class Or(left : Constraint, right : Constraint) extends Constraint