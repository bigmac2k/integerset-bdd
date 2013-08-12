package cc.sven.bdd

import cc.sven.memoize._
import scala.collection.mutable.WeakHashMap

sealed abstract trait BDD {
  val tag: Int
  val compl: Boolean
  def toString(c: Boolean): String
}

trait BDDLike[T] {
  val BDDTrue: T
  val BDDFalse: T
  def ite(t: T, e: T): T
  def unary_! : T
  def partialEval(bs: List[Boolean]): T
  def &&(that: T) = ite(that, BDDFalse)
  def ||(that: T) = ite(BDDTrue, that)
  def implies(that: T) = ite(that, BDDTrue)
}

class CBDD(val bdd: BDD, val compl: Boolean) extends BDDLike[CBDD] {
  val BDDTrue = True
  val BDDFalse = False
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
  def partialEval(bs: List[Boolean]): CBDD = (bs, this) match {
    case (_, False)                 => False
    case (_, True)                  => True
    case (true :: bs_, Node(t, _))  => t.partialEval(bs_)
    case (false :: bs_, Node(_, f)) => f.partialEval(bs_)
  }
  override def toString() = bdd.toString(compl)
  override def equals(that: Any) = that match {
    case t: CBDD => compl == t.compl && bdd == t.bdd
    case _       => false
  }
  override def hashCode = (bdd.hashCode, compl).hashCode
}
object CBDD {
  def apply(bits: List[Boolean]): CBDD = {
    require(bits match { case Nil => false; case _ => true })
      def helper(hbits: List[Boolean]): CBDD = hbits match {
        case true :: remBits  => Node(helper(remBits), False)
        case false :: remBits => Node(False, helper(remBits))
        case Nil              => True
      }
    helper(bits)
  }
}
class CBDDIterator(cbdd: CBDD, layers: Int) extends Iterator[List[Boolean]] {
  private var wl: List[(CBDD, List[Boolean])] = List((cbdd, List.empty))
  private var nextElem: Option[(List[Boolean], List[Boolean])] = None
  
  private def computeNext() {
    wl match {
      case Nil               => ()
      case (False, _) :: wl_ => computeNext()
      case (True, bs) :: wl_ => {
        val remaining = layers - bs.length
        nextElem = Some((bs, List.fill(remaining)(false)))
      }
      case (Node(set, uset), bs) :: wl_ => {
        wl = (set, true :: bs) :: (uset, false :: bs) :: wl_
        computeNext()
      }
    }
  }

  def hasNext(): Boolean = nextElem match {
    case None => {
      computeNext()
      nextElem match {
        case None    => false
        case Some(_) => hasNext()
      }
    }
    case Some(_) => true
  }
  
  def next() : List[Boolean] = nextElem match {
    case None => {
      computeNext()
      nextElem match {
        case None => throw new NoSuchElementException("next on empty iterator")
        case Some(_) => next()
      }
    }
    case Some((offs, lo)) => {
      val ret = (lo ++ offs).reverse
      succList(lo) match {
        case None => nextElem = None
        case Some(lo_) => nextElem = Some((offs, lo_))
      }
      ret
    }
  }
  
  private def succList(bs: List[Boolean]): Option[List[Boolean]] = bs match {
    case Nil          => None
    case false :: bs1 => Some(true :: bs1)
    case true :: bs1 => succList(bs1) match {
      case None      => None
      case Some(bs2) => Some(false :: bs2)
    }
  }
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
  private[this] val cache = WeakHashMap.empty[BDD, BDD]
  private[this] var tagCounter: Int = 1
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