package cc.sven.equivalences

import Either.Implicits._
import cc.sven.bdd.True
import scala.math.Ordering.Implicits._

/**
  * Created by scm on 04.02.16.
  * for java compat
  */
class EitherTupleSList[A : Ordering, B : Ordering, C : Ordering] protected (private val slist : SList[Either[(A, B), C]]) {
  def this() = this(SList.empty)

  def insert(a : Either[(A, B), C], b : Either[(A, B), C]) = new EitherTupleSList[A, B, C](slist.insert(a, b))

  def remove(a : Either[(A, B), C]) = new EitherTupleSList[A, B, C](slist.remove(a))

  def removeLefts()  = new EitherTupleSList[A, B, C](slist.filter(_.isRight))
  def removeRights() = new EitherTupleSList[A, B, C](slist.filter(_.isLeft))

  def removeLeftLeft(a : A) = new EitherTupleSList[A, B, C](slist.filter{
    case Left((a_, _)) => a != a_
    case _ => true
  })
  def removeLeftRight(b : B) = new EitherTupleSList[A, B, C](slist.filter{
    case Left((_, b_)) => b != b_
    case _ => true
  })
  /*def removeLeftRightRange(blo : B, bhi : B) = new EitherTupleSList[A, B, C](slist.filter{
    case Left((_, b_)) => b_ < blo || b_ > bhi
    case _ => true
  })*/
  def removeLeft(a : A, b : B) = new EitherTupleSList[A, B, C](slist.filter{
    case Left((a_, b_)) => a != a_ || b != b_
    case _ => true
  })
  def removeLeftRange(alo : A, ahi : A, blo : B, bhi : B) = new EitherTupleSList[A, B, C](slist.filter{
    case Left((a_, b_)) => (a_ < alo || a_ > ahi) || (b_ < blo || b_ > bhi)
    case _ => true
  })
  def removeRight(c : C) = new EitherTupleSList[A, B, C](slist.filter{
    case Right(c_) => c != c_
    case _ => true
  })

  def union(that : EitherTupleSList[A, B, C]) = new EitherTupleSList[A, B, C](slist.union(that.slist))

  def intersect(that : EitherTupleSList[A, B, C]) = new EitherTupleSList[A, B, C](slist.intersect(that.slist))

  def lookup(a : Either[(A, B), C]) = slist.lookup(a)

  override def toString = slist.toString

  def iterator() = slist.iterator()
}
