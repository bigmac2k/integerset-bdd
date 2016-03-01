package cc.sven.equivalences

import scala.math.Ordering.Implicits._
import cc.sven.bounded.DynBoundedBits
import cc.sven.tlike.Castable

/**
  * Created by scm on 10.02.16.
  */
sealed abstract class StorageEntity[R : Ordering, A : Ordering : DynBoundedBits, V : Ordering](implicit castALong : Castable[A, (Int, Long)]) {
  def overlapsWith(that : StorageEntity[R, A, V]) : Boolean
}
final case class MemLoc[R : Ordering, A : Ordering : DynBoundedBits, V : Ordering](val region : R, val address : A)
                                                                                  (implicit castALong : Castable[A, (Int, Long)]) extends StorageEntity[R, A, V] {
  val aDynBoundedBits = implicitly[DynBoundedBits[A]]
  private def memLocOverlap(a : MemLoc[R, A, V], b : MemLoc[R, A, V]) : Boolean = {
    val bytes = aDynBoundedBits.dBits(a.address) / 8
    val aLong = castALong(a.address)._2
    val bLong = castALong(b.address)._2
    bLong >= aLong && bLong <= aLong + bytes - 1
  }
  override def overlapsWith(that: StorageEntity[R, A, V]): Boolean = that match {
    case m@MemLoc(r, _) => region == r && (memLocOverlap(this, m) || memLocOverlap(m, this))
    case _ => false
  }
}
final case class Variable[R : Ordering, A : Ordering : DynBoundedBits, V : Ordering](val variable : V)
                                                                                    (implicit castALong : Castable[A, (Int, Long)]) extends StorageEntity[R, A, V] {
  override def overlapsWith(that: StorageEntity[R, A, V]): Boolean = that match {
    case Variable(v) => v == variable
    case _ => false
  }
}

object StorageEntity {
  def apply[R : Ordering, A : Ordering : DynBoundedBits, V : Ordering](r : R, a : A)(implicit castALong : Castable[A, (Int, Long)]) = MemLoc[R, A, V](r, a)
  def apply[R : Ordering, A : Ordering : DynBoundedBits, V : Ordering](v : V)(implicit castALong : Castable[A, (Int, Long)]) = Variable[R, A, V](v)
  def variableJ[R : Ordering, A : Ordering : DynBoundedBits, V : Ordering](v : V)(implicit castALongJ : Castable[A, cc.sven.misc.Pair[Integer, java.lang.Long]]) : Variable[R, A, V] = {
    implicit val castTIT = new Castable[A, (Int, Long)] {
      def apply(t : A) : (Int, Long) = {
        val temp = castALongJ(t)
        (temp._1, temp._2)
      }
    }
    apply[R, A, V](v)
  }
  def memLoc[R : Ordering, A : Ordering : DynBoundedBits, V : Ordering](r : R, a : A)(implicit castALongJ : Castable[A, cc.sven.misc.Pair[Integer, java.lang.Long]]) : MemLoc[R, A, V] = {
    implicit val castTIT = new Castable[A, (Int, Long)] {
      def apply(t : A) : (Int, Long) = {
        val temp = castALongJ(t)
        (temp._1, temp._2)
      }
    }
    apply[R, A, V](r, a)
  }
  object Implicits {
    implicit def storageEntityIsOrdering[R: Ordering, A: Ordering, V: Ordering] = new Ordering[StorageEntity[R, A, V]] {
      override def compare(x: StorageEntity[R, A, V], y: StorageEntity[R, A, V]): Int = (x, y) match {
        case (Variable(a), Variable(b)) => implicitly[Ordering[V]].compare(a, b)
        case (Variable(_), _) => -1
        case (MemLoc(ar, aa), MemLoc(br, ba)) if ar equiv br => implicitly[Ordering[A]].compare(aa, ba)
        case (MemLoc(ar, _), MemLoc(br, _)) => implicitly[Ordering[R]].compare(ar, br)
        case (MemLoc(_, _), _) => 1
      }
    }
  }
}
