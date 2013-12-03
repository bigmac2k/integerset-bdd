package cc.sven.constraint

import cc.sven.bounded._
import scala.collection.immutable.HashMap
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
  def solve[T, S[_]](table : HashMap[Int, S[T]])(implicit dBounded : DynBounded[T], ord : Ordering[T], const : Constrainable[T, S]) : HashMap[Int, S[T]] = {
    import ord.mkOrderingOps
    val varIds = getVarIds
    //under each variable id of this, there needs to be a non-empty set in table
    require(varIds.forall((x) => !const.isEmpty(table(x))))
    //Build a state including all values
    val allFull = (HashMap.empty[Int, S[T]] /: varIds){
      (acc, id) =>
        val setVal = const.min(table(id))
        val min = dBounded.dMinBound(setVal)
        val max = dBounded.dMaxBound(setVal)
        acc + ((id, const.range(min, max)))
    }
    def stateInvert(state : HashMap[Int, S[T]]) : HashMap[Int, S[T]] = state.map{case (k, v) => (k, const.invert(v))}
    val allEmpty = stateInvert(allFull)
    def valid(leftmin : Option[T], leftid : Int, leftmax : Option[T], rightmin : Option[T], rightid : Int, rightmax : Option[T], underapprox : Boolean) : HashMap[Int, S[T]] = {
      val left = const.min(table(leftid))
      val right = const.min(table(rightid))
      val leftMinBound = dBounded.dMinBound(left)
      val leftMaxBound = dBounded.dMaxBound(left)
      val rightMinBound = dBounded.dMinBound(right)
      val rightMaxBound = dBounded.dMaxBound(right)
      val leftmin_ = leftMinBound max leftmin.getOrElse(leftMinBound)
      val leftmax_ = leftMaxBound min leftmax.getOrElse(leftMaxBound)
      val rightmin_ = rightMinBound max rightmin.getOrElse(rightMinBound)
      val rightmax_ = rightMaxBound min rightmax.getOrElse(rightMaxBound)
      val validLeft = const.range(leftmin_, leftmax_)
      val validRight = const.range(rightmin_, rightmax_)
      (if(underapprox) allEmpty else allFull) + ((leftid, validLeft)) + ((rightid, validRight))
    }
    def buildAllValid(formula : Constraint, underapprox : Boolean) : HashMap[Int, S[T]] = (formula, underapprox) match {
      case (LTE(left, right), false) => valid(None, left, Some(const.max(table(right))), Some(const.min(table(left))), right, None, false)
      case (LTE(left, right), true) =>  valid(None, left, Some(const.min(table(right))), Some(const.max(table(left))), right, None, true)
      case (GTE(left, right), x) => stateInvert(buildAllValid(LTE(right, left), x))
      case (LT(left, right), x) => buildAllValid(GTE(left, right), !x)
      case (GT(left, right), x) => buildAllValid(LT(left, right), x)
      case (Equals(left, right), false) => {
        val res = const.intersect(table(left), table(right))
        allFull + ((left, res)) + ((right, res))
      }
      case (Equals(left, right), true) => {
        val res = const.intersect(table(left), table(right))
        if(const.min(res) == const.max(res))
          allFull + ((left, res)) + ((right, res))
        else
          allEmpty + ((left, const.invert(allEmpty(left)))) + ((right, const.invert(allEmpty(right))))
      }
      case (NEquals(left, right), x) => stateInvert(buildAllValid(Equals(left, right), !x))
      case (Not(op), x) => stateInvert(buildAllValid(op, !x))
      case (And(left, right), x) => buildAllValid(left, x).merged(buildAllValid(right, x)){
        case ((k1, v1), (k2, v2)) =>
          (k1, const.intersect(v1, v2))
      }
      case (Or(left, right), x) => stateInvert(buildAllValid(And(Not(left), Not(right)), !x))
    }
    //stub - should intersect table per value*/
    //XXX also be aware of empty sets! - return bottom (or do on java side)
    buildAllValid(this, false)
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