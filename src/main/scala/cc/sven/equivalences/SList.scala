package cc.sven.equivalences

import scala.math.Ordering.Implicits._

/**
  * Created by scm on 02.02.16.
  */
class SList[E : Ordering] protected (private val assocs : List[(E, E)]) extends Iterable[(E, E)] {
  def insert(a : E, b : E) : SList[E] = if(a == b) this else if(b < a) insert(b, a) else {
    new SList[E]((a, b) :: assocs.filter(p => p != (a, b)))
  }

  def filter(f : E => Boolean) : SList[E] = new SList[E](assocs.filter(p => f(p._1) && f(p._2)))

  def remove(e : E) : SList[E] = new SList[E](assocs.filter(p => p._1 != e && p._2 != e))

  def union(other : SList[E]) : SList[E] = new SList[E](assocs.union(other.assocs))

  def intersect(other : SList[E]) : SList[E] = new SList[E](assocs.intersect(other.assocs))

  def lookup(key : E) : List[E] = assocs.flatMap(t => if(t._1 == key) List(t._2) else if (t._2 == key) List(t._1) else Nil)

  override def toString = {
    val builder = new StringBuilder("SList(")
    def p(t : (E, E)) { builder.append(t._1); builder.append(" = "); builder.append(t._2) }
    def helper : List[(E, E)] => Unit = {
      case Nil => ()
      case List(a) => p(a)
      case x :: xs => p(x); builder.append(", "); helper(xs)
    }
    helper(assocs)
    builder.append(")")
    builder.mkString
  }

  def iterator() : Iterator[(E, E)] = assocs.iterator
}

object SList {
  def empty[E: Ordering] = new SList[E](Nil)

  def apply[E: Ordering](a: E, b: E): SList[E] = empty[E].insert(a, b)

  def apply[E: Ordering](es: (E, E)*): SList[E] = (empty[E] /: es) ((acc: SList[E], e: (E, E)) => acc.insert(e._1, e._2))
}
/*class SList[E : Ordering] protected (private val assocs : List[(E, E)]) extends Iterable[(E, E)] {
  def insert(a : E, b : E) : SList[E] = {
    if(a == b) this else {
      def sInsert(e : (E, E)) : List[(E, E)] => List[(E, E)] = {
        case Nil => List(e)
        case x :: xs if e == x => x :: xs
        case x :: xs if e > x => x :: sInsert(e)(xs)
        case x :: xs => e :: x :: xs
      }

      new SList[E](sInsert((b, a))(sInsert((a, b))(assocs)))
    }
  }

  def remove(e : E) : SList[E] = {
    def sRemove : List[(E, E)] => List[(E, E)] = {
      case Nil => Nil
      case (a, b) :: xs if a == e => xs
      case x :: xs => x :: sRemove(xs)
    }
    new SList[E](sRemove(assocs))
  }

  def filter(f : E => Boolean) : SList[E] = new SList[E](assocs.filter(p => f(p._1) && f(p._2)))

  def union(other : SList[E]) : SList[E] = {
    def sUnion : (List[(E, E)], List[(E, E)]) => List[(E, E)] = {
      case (Nil, b) => b
      case (a, Nil) => a
      case (a :: as, b :: bs) if a < b => a :: sUnion(as, b :: bs)
      case (a :: as, b :: bs) if a > b => b :: sUnion(a :: as, bs)
      case (a :: as, b :: bs) => a :: sUnion(as, bs)
    }
    new SList[E](sUnion(assocs, other.assocs))
  }

  def intersect(other : SList[E]) : SList[E] = {
    def sIntersect : (List[(E, E)], List[(E, E)]) => List[(E, E)] = {
      case (Nil, _) | (_, Nil) => Nil
      case (a :: as, b :: bs) if a < b => sIntersect(as, b :: bs)
      case (a :: as, b :: bs) if a > b => sIntersect(a :: as, bs)
      case (a :: as, b :: bs) => a :: sIntersect(as, bs)
    }
    new SList[E](sIntersect(assocs, other.assocs))
  }

  def lookup(key : E) : List[E] = assocs.dropWhile(t => t._1 < key).takeWhile(t => t._1 == key).map(_._2)

  override def toString = "SList(" + assocs.toString() + ")"

  def iterator() : Iterator[(E, E)] = assocs.iterator
}
}*/
