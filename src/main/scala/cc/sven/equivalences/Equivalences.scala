package cc.sven.equivalences

import cc.sven.bounded.DynBoundedBits
import cc.sven.tlike.Castable
import scala.collection.JavaConverters._
import scala.math.Ordering.Implicits._
import StorageEntity.Implicits._

/**
  * Created by scm on 10.02.16.
  */
class Equivalences[R : Ordering, A : Ordering : DynBoundedBits, V : Ordering] protected (private val list : List[(StorageEntity[R, A, V], StorageEntity[R, A, V])])
                                                                                        (implicit castALong : Castable[A, (Int, Long)])
                                                                                        extends Iterable[(StorageEntity[R, A, V], StorageEntity[R, A, V])] {
  def this()(implicit castALong : Castable[A, (Int, Long)]) = this(List())
  private type STuple = (StorageEntity[R, A, V], StorageEntity[R, A, V])
  def insert(storageA : StorageEntity[R, A, V], storageB : StorageEntity[R, A, V]) : Equivalences[R, A, V] = {
    def tuple[A : Ordering](a : A, b : A) = if(a <= b) (a, b) else (b, a)
    val newEq@(na, nb) = tuple(storageA, storageB)
    val (filtered, eqs) = ((List[STuple](), List[STuple]()) /: list) { case ((f, e), x@(a, b)) =>
      if(x equiv newEq) return this //equivalence present, ignore insert
      val doesOverlap = a.overlapsWith(storageA) || b.overlapsWith(storageA)
      val filtered_  = if(doesOverlap) f else x :: f
      val eqs_ = if(!doesOverlap) {
        if(nb equiv a) tuple(na, b) :: e else
        if(na equiv a) tuple(nb, b) :: e else
        if(nb equiv b) tuple(na, a) :: e else
        if(na equiv b) tuple(nb, a) :: e else
        e
      } else e
      (filtered_, eqs_)
    }
    new Equivalences[R, A, V](newEq :: eqs ++ filtered)
  }
  def remove(storage : StorageEntity[R, A, V]) = new Equivalences[R, A, V](list.filter{ case (a, b) =>
      !storage.overlapsWith(a) && !storage.overlapsWith(b)
  })
  def removeAllMemLocs() = new Equivalences[R, A, V](list.filter {
    case (MemLoc(_, _), _) | (_, MemLoc(_, _)) => false
    case _ => true
  })
  def lookup(storage : StorageEntity[R, A, V]) : List[StorageEntity[R, A, V]] = list.flatMap{
    case (a, b) if storage equiv a => List(b)
    case (a, b) if storage equiv b => List(a)
    case _ => Nil
  }
  def lookupJava(storage : StorageEntity[R, A, V]) = lookup(storage).asJava
  def union(that : Equivalences[R, A, V]) = new Equivalences[R, A, V](list.union(that.list))
  def intersect(that : Equivalences[R, A, V]) = new Equivalences[R, A, V](list.intersect(that.list))

  override def iterator : Iterator[(StorageEntity[R, A, V], StorageEntity[R, A, V])] = list.iterator
  def javaIterator = iterator.asJava
  override def toString = {
    val builder = new StringBuilder("Eqs(")
    def helper : List[STuple] => Unit = {
      case Nil => ()
      case (a, b) :: Nil => builder.append(a.toString); builder.append(" = "); builder.append(b.toString)
      case (a, b) :: xs => builder.append(a.toString); builder.append(" = "); builder.append(b.toString); builder.append(", "); helper(xs)
    }
    helper(list)
    builder.append(")")
    builder.mkString
  }
}

object Equivalences {
  def apply[R : Ordering, A : Ordering : DynBoundedBits, V : Ordering](xs : (StorageEntity[R, A, V], StorageEntity[R, A, V])*)
                                                                      (implicit castALong : Castable[A, (Int, Long)]) =
    (new Equivalences[R, A, V]() /: xs){ case (acc, (a, b)) => acc.insert(a, b) }
  def apply[R : Ordering, A : Ordering : DynBoundedBits, V : Ordering]()
                                                                      (implicit castALong : Castable[A, (Int, Long)]) = new Equivalences[R, A, V]()
  def applyJ[R : Ordering, A : Ordering : DynBoundedBits, V : Ordering](implicit castALongJ : Castable[A, cc.sven.misc.Pair[Integer, java.lang.Long]]) : Equivalences[R, A, V] = {
    implicit val castTIT = new Castable[A, (Int, Long)] {
      def apply(t : A) : (Int, Long) = {
        val temp = castALongJ(t)
        (temp._1, temp._2)
      }
    }
    apply[R, A, V]()
  }
}
