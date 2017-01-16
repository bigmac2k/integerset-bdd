package cc.sven.bdd

import scala.collection.mutable.WeakHashMap
import scala.ref._
import cc.sven.misc.unsignedLongToBigInt
/*
import scala.concurrent._
import scala.concurrent.duration.Duration.Inf
import scala.concurrent.ExecutionContext.Implicits.global
*/

sealed abstract trait BDD {
  val depth: Int
  val count: Long
  val nodecount : Long
  val tag: Int
  val compl: Boolean
  def toString(c: Boolean): String
}

class CBDD(val bdd: BDD, val compl: Boolean) {
  def unary_! = new CBDD(bdd, !compl)
  def depth = bdd.depth
  def count : Long = {
    import scala.math.BigInt._
    if(compl)
      if(bdd.depth == 64)
        //bdd.count will at least be 1 as only False has 0 and it has depth != 64
        //interpretation is as unsigned long...
        (((1 : BigInt) << bdd.depth) - (bdd.count : BigInt)).longValue
      else
        (1l << bdd.depth) - bdd.count
    else
      bdd.count
  }
  def nodecount = bdd.nodecount
  private[this] def ite_raw( /*depth : Int,*/ triple: (CBDD, CBDD, CBDD), f: ( /*Int,*/ (CBDD, CBDD, CBDD)) => CBDD): CBDD =
    triple match {
      case (_, t, e) if t == e     => t
      case (True, t, _)            => t
      case (False, _, e)           => e
      case (i, True, False)        => i
      case (i, False, True)        => !i
      case (i, True, e) if i == e  => i
      case (i, False, e) if i == e => False
      case (i, True, e) if i == !e => True
      case (i, False, e) if i == !e => !i
      case (i, t, False) if i == t => i
      case (i, t, True) if i == t  => True
      case (i, t, False) if i == !t => False
      case (i, t, True) if i == !t => !i
      case (Node(iset, iuset), t, e) => {
          def extract(cbdd: CBDD) = cbdd match {
            case True            => (True, True)
            case False           => (False, False)
            case Node(set, uset) => (set, uset)
          }
        val (tset, tuset) = extract(t)
        val (eset, euset) = extract(e)
        //XXX remove
        /*if(true) {*/
        //println("nonpar " + depth.toString)
        Node(f( /*depth + 1,*/ (iset, tset, eset)), f( /*depth + 1,*/ (iuset, tuset, euset)))
        /*} else {
          //println("par " + depth.toString)
          val nset = future(f(depth + 1, (iset, tset, eset)))
          val nuset = future(f(depth + 1, (iuset, tuset, euset)))
          Node(Await.result(nset, Inf), Await.result(nuset, Inf))
        }*/
      }
    }
  //private[this] val memIte = Memoized.apply[(CBDD, CBDD, CBDD), CBDD](ite_raw(_, _))
  private[this] var recIte: ( /*Int,*/ (CBDD, CBDD, CBDD)) => CBDD = null
  private[this] val memIte: ( /*Int,*/ (CBDD, CBDD, CBDD)) => CBDD = ite_raw(_, recIte) //(a, b) => ite_raw(a, b, recIte)
  recIte = memIte
  def ite(t: CBDD, e: CBDD): CBDD = memIte( /*0,*/ (this, t, e))
  def &&(that: CBDD) = ite(that, False)
  def ||(that: CBDD) = ite(True, that)
  def implies(that: CBDD) = ite(that, True)
  def partialEval(bs: List[Boolean]): Option[CBDD] = (bs, this) match {
    case (List(), _) => Some(this)
    case (_, False)                 => Some(False)
    case (_, True)                  => Some(True)
    case (true :: bs_, Node(t, _))  => t.partialEval(bs_)
    case (false :: bs_, Node(_, f)) => f.partialEval(bs_)
    case _ => None
  }
  def trueMost: Option[List[Boolean]] = this match {
    case False             => None
    case True              => Some(Nil)
    case Node(False, uset) => uset.trueMost.map(false :: _)
    case Node(set, _)      => set.trueMost.map(true :: _)
  }
  def falseMost: Option[List[Boolean]] = this match {
    case False            => None
    case True             => Some(Nil)
    case Node(set, False) => set.falseMost.map(true :: _)
    case Node(_, uset)    => uset.falseMost.map(false :: _)
  }
  private def computeTruePaths(path: List[Boolean]): Stream[List[Boolean]] = this match {
    case False           => Stream()
    case True            => Stream(path)
    case Node(set, uset) => set.computeTruePaths(true :: path) #::: uset.computeTruePaths(false :: path)
  }
  def truePaths = computeTruePaths(List()).map(_.reverse)
  def randomTruePath() = {
      def helper(bdd: CBDD, path: List[Boolean]): List[Boolean] = bdd match {
        case True              => path
        case False             => throw new NoSuchElementException("randomTruePath on empty CBDD")
        case Node(False, uset) => helper(uset, false :: path)
        case Node(set, False)  => helper(set, true :: path)
        case Node(set, uset) => {
          val dir = scala.util.Random.nextBoolean()
          if (dir) helper(set, true :: path) else helper(uset, false :: path)
        }
      }
    helper(this, Nil).reverse
  }
  def doesImply(that: CBDD): Boolean = (this, that) match {
    case (a, b) if a == b                       => true
    case (Node(set1, uset1), Node(set2, uset2)) => set1.doesImply(set2) && uset1.doesImply(uset2)
    case (False, False)                         => true
    case (True, True)                           => true
    case (_, True)                              => true
    case (False, _)                             => true
    case _                                      => false
  }
  def take(toTake: Int): CBDD = this match {
    case False            => False
    case _ if toTake <= 0 => True
    case Node(set, uset)  => Node(set.take(toTake - 1), uset.take(toTake - 1))
    case True             => True
  }
  def drop[A](toDrop: Int, acc : A, mapF : CBDD => A, reduceF : (A, A) => A): A = this match {
    case False            => mapF(False)
    case _ if toDrop <= 0 => mapF(this)
    case Node(set, uset) => {
      val setDropped = set.drop(toDrop - 1, acc, mapF, reduceF)
      val usetDropped = uset.drop(toDrop - 1, acc, mapF, reduceF)
      reduceF(setDropped, usetDropped)
    }
    case True => mapF(True)
  }
  def dropOr(toDrop : Int) = this.drop(toDrop, False, (x : CBDD) => x, (a : CBDD, b : CBDD) => a || b)
  def replaceWith(toReplace : CBDD, toReplaceWith : CBDD) : CBDD = if(this == toReplace) toReplaceWith else this match {
    case True => True
    case False => False
    case Node(set, uset) if set == uset => {
      val sub = set.replaceWith(toReplace, toReplaceWith)
      Node(sub, sub)
    }
    case Node(set, uset) => Node(set.replaceWith(toReplace, toReplaceWith), uset.replaceWith(toReplace, toReplaceWith))
  }
  override def toString = bdd.toString(compl)
  override def equals(that: Any) = that match {
    case t: CBDD => compl == t.compl && (bdd eq t.bdd)
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
  def apply(path: List[Boolean], set: CBDD, uset: CBDD, terminal: CBDD): CBDD = path match {
    case Nil            => terminal
    case true :: path_  => Node(CBDD(path_, set, uset, terminal), uset)
    case false :: path_ => Node(set, CBDD(path_, set, uset, terminal))
  }
  def apply(path1 : List[Boolean], path2 : List[Boolean]) : CBDD = {
    def checker(p1 : List[Boolean], p2 : List[Boolean]) : Boolean = (p1, p2) match {
      case (true :: p1_, true :: p2_) => checker(p1_, p2_)
      case (false :: p1_, false :: p2_) => checker(p1_, p2_)
      case (false :: _, true :: _) => true
      case (Nil, Nil) => true
      case _ => false
    }
    require(checker(path1, path2))
    apply(path1, True, False, True) && apply(path2, False, True, True)
  }
  def union3(a: CBDD, b: CBDD, c: CBDD): CBDD = List(a, b, c).distinct.reduce(_ || _)
  type CBDDTuple = (CBDD, CBDD)
  def addMerge(ff: CBDDTuple, ft: CBDDTuple, tf: CBDDTuple, tt: CBDDTuple): CBDDTuple = {
    val trueOVNot = union3(tf._1, ft._1, ff._2)
    val falseOVNot = ff._1
    val trueOV = tt._2
    val falseOV = union3(tt._1, tf._2, ft._2)
    (Node(trueOVNot, falseOVNot), Node(trueOV, falseOV))
  }
  private def pred(integer : List[Boolean]) : List[Boolean] = {
    def helper(bs : List[Boolean]) : (Boolean, List[Boolean]) = bs match {
      case List(x) => (x, List(!x))
      case b :: bs => {
        val (flipped, rec) = helper(bs)
        (b, (b == flipped) :: rec)
      }
    }
    helper(integer)._2
  }
  private def pred2(integer : List[Boolean]) : List[Boolean] = {
    def helper(bs : List[Boolean]) : (Boolean, List[Boolean]) = bs match {
      case List(x) => (x, List(!x))
      case b :: bs => {
        val (carry, rec) = helper(bs)
        (carry || b, (b == carry) :: rec)
      }
    }
    helper(integer)._2
  }
  def plus(op1: CBDD, op2: CBDD, depth: Int): CBDDTuple = (op1, op2) match {
    case (False, _) => (False, False)
    case (x, False) => plus(False, x, depth)
    case (True, True) => {
      val ov = if (depth == 0) False else !CBDD(List.fill(depth)(true))
      (True, ov)
    }
    case (n@Node(set, uset), True) => {
      val lo = n.falseMost.get.padTo(depth, false)
      val hi = n.trueMost.get.padTo(depth,true)
      val noOv = CBDD(lo, List.fill(depth)(true))
      val ov = if(lo.forall(!_)) False else CBDD(List.fill(depth)(false), pred(hi))
      (noOv, ov)
    }
    /* less efficient version
     *case (Node(set, uset), True) => {
     *  val tt = plus(set, True, depth - 1)
     *  val tf = tt
     *  val ff = plus(uset, True, depth - 1)
     *  val ft = ff
     *  addMerge(ff, ft, tf, tt)
     *}
     */
    case (True, x) => plus(x, True, depth)
    case (Node(set1, uset1), Node(set2, uset2)) => {
      /*optimization:
       * op1 == op2 -> op1 << 1 XXX
       * set1 == uset1 -> tt = ft, tf = ff
       * set2 == uset2 -> tt = tf, ft = ff
       */
      val tt = plus(set1, set2, depth - 1)
      val ft = if (set1 == uset1) tt else plus(uset1, set2, depth - 1)
      val ff = if (set2 == uset2) ft else plus(uset1, uset2, depth - 1)
      val tf = if (set1 == uset1) ff else if (set2 == uset2) tt else plus(set1, uset2, depth - 1)
      addMerge(ff, ft, tf, tt)
    }
  }
  // this is probably not correct
  def plusSingleton(op1: CBDD, op2: List[Boolean], depth: Int): CBDDTuple = {

    (op1, op2) match {
      case (False, _) => (False, False)
      case (True, Nil) =>
        val ov = if (depth == 0) False else !CBDD(List.fill(depth)(true))
        (True, ov)
      case (True, _) => {
        val noOv = CBDD(op2, List.fill(depth)(true))
        val ov = if(op2.forall(!_)) False else CBDD(List.fill(depth)(false), pred2(op2)) // ???
        (noOv, ov)
      }
      case (Node(set, uset), x :: xs) => {
//        require(op1.depth == op2.length, s"Operands aren't of equal length: ${op1.depth} vs ${op2.length}")
        val (tNoOv, tOv) = plusSingleton(set, xs, depth - 1) // (01,10)
        val (fNoOv, fOv) = plusSingleton(uset, xs, depth - 1) // (00,01)
        if (x) { // 1
        val trueOVNot = fNoOv
          val falseOVNot = False
          val trueOV = tOv
          val falseOV = tNoOv || fOv
          (Node(trueOVNot, falseOVNot), Node(trueOV, falseOV))
        } else { // 0
        val trueOVNot = tNoOv || fOv
          val falseOVNot = fNoOv
          val trueOV = False
          val falseOV = tOv
          (Node(trueOVNot, falseOVNot), Node(trueOV, falseOV))
        }
      }
    }
  }

  def negate(bits : Int, bdd : CBDD) = {
    val (ov, norm) = plus(bNot(bdd), (CBDD(List.fill(bits - 1)(false) ++ List(true))), bits)
    ov || norm
  }
  private def bitwiseOpHelper(op: (Boolean, Boolean) => Boolean)(trueFalseTuples: List[((CBDD, Boolean), (CBDD, Boolean))]): CBDD = {
    val trueFalse = trueFalseTuples.distinct.partition((t) => op(t._1._2, t._2._2))
    val nset = ((False: CBDD) /: trueFalse._1)((acc, t) => acc || bitwiseOp(op)(t._1._1, t._2._1))
    val nuset = ((False: CBDD) /: trueFalse._2)((acc, t) => acc || bitwiseOp(op)(t._1._1, t._2._1))
    Node(nset, nuset)
  }
  //XXX should probably be specialized
  def bitwiseOp(op: (Boolean, Boolean) => Boolean)(op1: CBDD, op2: CBDD): CBDD = (op1, op2) match {
    case (False, _)   => False
    case (x, False)   => bitwiseOp(op)(False, x)
    case (True, True) => True
    case (Node(set, uset), True) => {
      val trueFalseTuples = List(((set, true), (True, true)), ((set, true), (True, false)), ((uset, false), (True, true)), ((uset, false), (True, false)))
      bitwiseOpHelper(op)(trueFalseTuples)
    }
    case (True, Node(set, uset)) => { //list this case explicitly to not force op to be commutative
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
  def bAnd(op1: CBDD, op2: CBDD): CBDD = (op1, op2) match {
    case (False, _) => False
    case (_, False) => bAnd(op2, op1)
    case (True, x) => {
        def trueRecurse(bdd: CBDD): CBDD = bdd match {
          case False => False
          case True  => True
          case Node(set, uset) => {
            val nset = trueRecurse(set)
            val nuset = trueRecurse(uset) || nset
            Node(nset, nuset)
          }
        }
      trueRecurse(x)
    }
    case (_, True) => bAnd(op2, op1)
    case (Node(set1, uset1), Node(set2, uset2)) => {
      val tt = bAnd(set1, set2)
      val ft = if(set1 == uset1) tt else bAnd(uset1, set2)
      val ff = if(set2 == uset2) ft else bAnd(uset1, uset2)
      val tf = if(set1 == uset1) ff else if (set2 == uset2) tt else bAnd(set1, uset2)
      val nuset = union3(tf, ft, ff)
      Node(tt, nuset)
    }  
  }
  def bOr(op1 : CBDD, op2 : CBDD) : CBDD = (op1, op2) match {
    case (False, _) => False
    case (_, False) => bOr(op2, op1)
    case (True, x) => {
      def trueRecurse(bdd : CBDD) : CBDD = bdd match {
        case False => False
        case True => True
        case Node(set, uset) => {
          val nuset = trueRecurse(uset)
          val nset = trueRecurse(set) || nuset
          Node(nset, nuset)
        }
      }
      trueRecurse(x)
    }
    case (_, True) => bOr(op2, op1)
    case (Node(set1, uset1), Node(set2, uset2)) => {
      val tt = bOr(set1, set2)
      val ft = if(set1 == uset1) tt else bOr(uset1, set2)
      val ff = if(set2 == uset2) ft else bOr(uset1, uset2)
      val tf = if(set1 == uset1) ff else if(set2 == uset2) tt else bOr(set1, uset2)
      val nset = union3(tt, ft, tf)
      Node(nset, ff)
    }
  }
  def bXOr(op1 : CBDD, op2 : CBDD) : CBDD = (op1, op2) match {
    case (False, _) => False
    case (_, False) => bXOr(op2, op1)
    case (True, _) => True
    case (_, True) => bXOr(op2, op1)
    case (Node(set1, uset1), Node(set2, uset2)) => {
      val tt = bXOr(set1, set2)
      val ft = if(set1 == uset1) tt else bXOr(uset1, set2)
      val ff = if(set2 == uset2) ft else bXOr(uset1, uset2)
      val tf = if(set1 == uset1) ff else if(set2 == uset2) tt else bXOr(set1, uset2)
      val nset = ft || tf
      val nuset = tt || ff
      Node(nset, nuset)
    }
  }
  def bNot(op1: CBDD): CBDD = op1 match {
    case False           => False
    case True            => True
    case Node(set, uset) if set == uset => {
      val not = bNot(set)
      Node(not, not)
    }
    case Node(set, uset) => Node(bNot(uset), bNot(set))
  }

  def constructStridedInterval(start: Long, count: Long, stride : Long, height : Int) : CBDD = {
    val stride_ = stride.abs

    def helper2(toBeConsumed: Long, h: Long, length: Long): (CBDD, Long, Long) = {
     // require(toBeConsumed >= 0)

      if (length <= 0) return (False, toBeConsumed, 0)
      if (h == 0 && toBeConsumed == 0) return (True, stride_ - 1, 1)
      val remaining = if (h < 64) toBeConsumed - (1L << h) else -1 // overflow, if h>63 we can consume any 0<=x<=long.MaxValue

      if (remaining < 0L) {
        val (bddF, leftF, countF) = helper2(toBeConsumed, h - 1, length)
        val (bddT, leftT, countT) = helper2(leftF, h - 1, length - countF)
        (Node(bddT, bddF), leftT, countF + countT)
      } else {
        (False, remaining, 0)
      }
    }

    val maxCount = count // (end - start).abs / stride_ + 1
    var start_ = start // if (start >= 0) start else start + (1L << (height - 1)) + (1L << (height - 1))// Math.min(start, end)
    var totalCount = 0L
    var result: CBDD = False
    do {

      if (start_ < 0) {
        val start__ = start_ + (1L << (height - 2)) + (1L << (height - 2)) // not in right subtree. TODO wrap around
        val (res, left, c) = helper2(start__, height - 1, maxCount)
        result = result || Node(res, False)
        totalCount += c
        start_ = left
        /*if (c < maxCount) {
          val (res2, left2, c2) = helper2(left, height - 1, maxCount - c)
          totalCount += c2
          result = result || Node(res, res2)
        } else {
          result = result || Node(res, False)
        }
        */
      } else {
        val (res, left, c) = helper2(start_, height, maxCount - totalCount)
        result = result || res
        totalCount += c
        start_ = left
      }
    } while (totalCount < count)
    result
  }
}

class CBDDIterator(cbdd: CBDD, layers: Int) extends Iterator[List[Boolean]] {
  private var wl: List[(CBDD, List[Boolean])] = List((cbdd, List.empty))
  private var nextElem: Option[(List[Boolean], List[Boolean])] = None

  private def computeNext() {
    wl match {
      case Nil => ()
      case (False, _) :: wl_ => {
        wl = wl_
        computeNext()
      }
      case (True, bs) :: wl_ => {
        val remaining = layers - bs.length
        wl = wl_
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

  def next(): List[Boolean] = nextElem match {
    case None => {
      computeNext()
      nextElem match {
        case None    => throw new NoSuchElementException("next on empty iterator")
        case Some(_) => next()
      }
    }
    case Some((offs, lo)) => {
      val ret = (lo ++ offs).reverse
      succList(lo) match {
        case None      => nextElem = None
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
  val depth: Int = 0
  val tag: Int = 0
  val count: Long = 1
  val nodecount: Long = 1
  val compl: Boolean = false
  def toString(c: Boolean) = if (c) "False(" + tag + ", " + depth + ", " + 0 + ")" else "True(" + tag + ", " + depth + ", " + 1 + ")"
  override def hashCode = tag
}

object True extends CBDD(Terminal, false) {
  def unapply(cbdd: CBDD) = if (cbdd.bdd == Terminal && !cbdd.compl) Some(cbdd) else None
}
object False extends CBDD(Terminal, true) {
  def unapply(cbdd: CBDD) = if (cbdd.bdd == Terminal && cbdd.compl) Some(cbdd) else None
}

final class Node(val set: BDD, val uset: BDD, val compl: Boolean, val tag: Int) extends BDD {
  val depth = scala.math.max(set.depth, uset.depth) + 1
  private def countR: Long = uset match {
    case Terminal if(compl) => 0
    case Terminal => 1
    case _ => if(compl) (1l << uset.depth) - uset.count else uset.count
  }
  
  private def countL: Long = set.count
  val count = if (uset.depth > set.depth)
      (1l << (uset.depth - set.depth)) * countL + countR
    else
      (1l << (set.depth - uset.depth)) * countR + countL
  //def toString(c: Boolean) = "Node(" + set.toString(c) + ", " + uset.toString(c != compl) + ", " + tag.toString + ", " + count + ", " + depth + ")"
  def toString(c: Boolean) = "Node(" + tag + ", " + depth + ", " + count + ", " + set.toString(c) + ", " + uset.toString(c != compl) + ")"
  override def hashCode = (set.tag, uset.tag, compl).hashCode
  override def equals(that: Any) = that match {
    case t: Node => compl == t.compl && (set eq t.set) && (uset eq t.uset)
    case _       => false
  }
  val nodecount = {
    val res = set.nodecount + uset.nodecount
    //-1 indicates that overflow happend over 0 (overflow in unsigned long)
    if(set.nodecount >= 0 && uset.nodecount >= 0 || res < 0) res else -1
  }
}
object Node {
  //this is potentially terrible. Values make keys strongly referenced?
  private[this] val cache = WeakHashMap.empty[BDD, WeakReference[BDD]]
  private[this] var tagCounter: Int = 1
  def cacheSize() = cache.size
  def nodeCount() = ((0 : BigInt, false) /: cache.toList){ case ((n, huge), (bdd, _)) =>
    if(bdd.nodecount == -1) (n, true) else (unsignedLongToBigInt(bdd.nodecount) + n, huge)
  }
  def status(): String = {
    val nc = nodeCount()
    "Tag counter: " + tagCounter + "; Items in cache: " + cacheSize() + (if(cacheSize() != 0) "; Nodes in cache: " + nc._1 + "; Average BDD size: " + (nc._1 / cacheSize()) + (if(nc._2) "; There where huge uncounted BDDs" else "") else "")
  }
  def apply(set: CBDD, uset: CBDD) = {
    val ibit = set.compl
    if (set.compl == uset.compl && set.bdd == Terminal && uset.bdd == Terminal) new CBDD(Terminal, ibit) else {
      val usetbit = set.compl != uset.compl
      val tentative = new Node(set.bdd, uset.bdd, usetbit, tagCounter)
      val hashconsed = cache.getOrElseUpdate(tentative, WeakReference(tentative)).get.getOrElse(tentative)
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
