package cc.sven.bdd

import cc.sven.memoize._
import scala.collection.mutable.WeakHashMap

sealed abstract trait BDD {
  val tag: Int
  val compl: Boolean
  def toString(c: Boolean): String
}

trait BDDLike {
  def ite(t : CBDD, e : CBDD) : CBDD
  def unary_! : CBDD
  def &&(that: CBDD) = ite(that, False)
  def ||(that: CBDD) = ite(True, that)
  def implies(that: CBDD) = ite(that, True)
}

class CBDD(val bdd: BDD, val compl: Boolean) extends BDDLike {
  def unary_! = new CBDD(bdd, !compl)
  private[this] def ite_raw(triple: (CBDD, CBDD, CBDD), f: ((CBDD, CBDD, CBDD)) => CBDD): CBDD =
    triple match {
      case (_, t, e) if t == e => t
      case (True, t, _)        => t
      case (False, _, e)       => e
      case (i, True, False)    => i
      case (i, False, True)    => !i
      case (Node(iset, iuset), t, e) => {
          def extract(cbdd: CBDD) = cbdd match {
            case True            => (True, True)
            case False           => (False, False)
            case Node(set, uset) => (set, uset)
          }
        val (tset, tuset) = extract(t)
        val (eset, euset) = extract(e)
        Node(f(iset, tset, eset), f(iuset, tuset, euset))
      }
    }
  private[this] val memIte = Memoized.apply[(CBDD, CBDD, CBDD), CBDD](ite_raw(_, _))
  def ite(t: CBDD, e: CBDD): CBDD = memIte(this, t, e)
  override def toString() = bdd.toString(compl)
  override def equals(that: Any) = that match {
    case t: CBDD => compl == t.compl && bdd == t.bdd
    case _       => false
  }
  override def hashCode = (bdd.hashCode, compl).hashCode
}

object Terminal extends BDD {
  val tag: Int = 0
  val compl: Boolean = false
  def toString(c: Boolean) = if (c) "False" else "True"
  override def hashCode = tag
}

object True extends CBDD(Terminal, false) {
  def unapply(cbdd: CBDD) = if (cbdd.bdd == Terminal && !cbdd.compl) Some(cbdd) else None
}
object False extends CBDD(Terminal, true) {
  def unapply(cbdd: CBDD) = if (cbdd.bdd == Terminal && cbdd.compl) Some(cbdd) else None
}

final class Node(val set: BDD, val uset: BDD, val compl: Boolean, val tag: Int) extends BDD {
  def toString(c: Boolean) = "Node(" + set.toString(c) + ", " + uset.toString(c != compl) + ", " + tag.toString + ")"
  override def hashCode = (set.tag, uset.tag, compl).hashCode
  override def equals(that: Any) = that match {
    case t: Node => compl == t.compl && (set eq t.set) && (uset eq t.uset)
    case _       => false
  }
}
object Node {
  val cache = WeakHashMap.empty[BDD, BDD]
  var tagCounter: Int = 1
  def status(): String = "Items in cache: " ++ cache.size.toString
  def apply(set: CBDD, uset: CBDD) = {
    val ibit = set.compl
    if (set.compl == uset.compl && set.bdd == Terminal && uset.bdd == Terminal) new CBDD(Terminal, ibit) else {
      val usetbit = set.compl != uset.compl
      val tentative = new Node(set.bdd, uset.bdd, usetbit, tagCounter)
      val hashconsed = cache.getOrElseUpdate(tentative, tentative)
      if (tentative eq hashconsed) tagCounter += 1;
      new CBDD(hashconsed, ibit)
    }
  }
  def unapply(cbdd: CBDD) = cbdd.bdd match {
    case b: Node => {
      val setbit = cbdd.compl
      val usetbit = cbdd.compl != b.compl
      Some((new CBDD(b.set, setbit), new CBDD(b.uset, usetbit)))
    }
    case _ => None
  }
}