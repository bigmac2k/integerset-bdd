package cc.sven.bdd

import scala.collection.mutable.WeakHashMap
import scala.ref._
import cc.sven.misc.unsignedLongToBigInt

/** Trait of BDD objects.
  * BDD objects have a depth, specifying the total depth of the subtree,
  * a count of total paths to True terminals,
  * a nodecount, specifying how many nodes exist in the subtree,
  * a tag, (TODO)
  * and a complement bit which can be set to invert the tree.
  * Supplies toString method.
  * Used by terminal. Used by Node.
  */
sealed abstract trait BDD {
  val depth: Int
  val count: Long
  val nodecount: Long
  val tag: Int
  val compl: Boolean

  def toString(c: Boolean): String
}

// TODO
/** Class of CBDD with additional complement information. Used by root nodes.
  * Defines operations with complement option.
  * Uses helper method ite to implement operations.
  *
  * @param bdd   BDD object
  * @param compl Boolean, specifying if BDD is inverted
  */
class CBDD(val bdd: BDD, val compl: Boolean) {
  def unary_! = new CBDD(bdd, !compl)

  def depth = bdd.depth

  def count: Long = {
    import scala.math.BigInt._
    if (compl)
      if (bdd.depth == 64)
      //bdd.count will at least be 1 as only False has 0 and it has depth != 64
      //interpretation is as unsigned long...
        (((1: BigInt) << bdd.depth) - (bdd.count: BigInt)).longValue
      else
        (1l << bdd.depth) - bdd.count
    else
      bdd.count
  }

  def nodecount = bdd.nodecount

  private[this] def ite_raw(triple: (CBDD, CBDD, CBDD), f: ((CBDD, CBDD, CBDD)) => CBDD): CBDD =
    triple match {
      case (_, t, e) if t == e => t
      case (True, t, _) => t
      case (False, _, e) => e
      case (i, True, False) => i
      case (i, False, True) => !i
      case (i, True, e) if i == e => i
      case (i, False, e) if i == e => False
      case (i, True, e) if i == !e => True
      case (i, False, e) if i == !e => !i
      case (i, t, False) if i == t => i
      case (i, t, True) if i == t => True
      case (i, t, False) if i == !t => False
      case (i, t, True) if i == !t => !i
      case (Node(iset, iuset), t, e) => {
        def extract(cbdd: CBDD) = cbdd match {
          case True => (True, True)
          case False => (False, False)
          case Node(set, uset) => (set, uset)
        }
        val (tset, tuset) = extract(t)
        val (eset, euset) = extract(e)
        Node(f((iset, tset, eset)), f((iuset, tuset, euset)))
      }
    }

  private[this] var recIte: ((CBDD, CBDD, CBDD)) => CBDD = null
  private[this] val memIte: ((CBDD, CBDD, CBDD)) => CBDD = ite_raw(_, recIte)
  recIte = memIte

  /** If-then-else function for operation on CBDDs. Used to implement any two-variable logic function.
    * Given two CBDDs, representing logical expressions, evaluates the expression represented by the calling object,
    * for each Node deciding which branch is taken (then or else), constructing the appropriate CBDD object and
    * returning it.
    *
    * Functionally used for implementation of binary operations (such as AND, OR, IMPLIES), where for each pair
    * of Nodes a function is applied. Either a Terminal is found or a recursive call on the remainder is made.
    * The result of each application is then stored in the CBDD which is returned.
    *
    * @param t CBDD of then
    * @param e CBDD of else
    * @return CBDD of computed result
    */
  def ite(t: CBDD, e: CBDD): CBDD = memIte((this, t, e))

  /** Logical And for CBDDs. */
  def &&(that: CBDD) = ite(that, False)

  /** Logical Or for CBDDs. */
  def ||(that: CBDD) = ite(True, that)

  /** Logical Implies for CBDDs. */
  def implies(that: CBDD) = ite(that, True)

  /** Partially evaluates a CBDD. Given a list of Booleans, representing a path, follows the path through
    * the CBDD, returning a Some(Terminal) if a Terminal is found in the path, else None.
    *
    * @param bs list of Booleans representing a path
    * @return an Option of Terminal in path
    */
  def partialEval(bs: List[Boolean]): Option[CBDD] = (bs, this) match {
    case (List(), _) => Some(this)
    case (_, False) => Some(False)
    case (_, True) => Some(True)
    case (true :: bs_, Node(t, _)) => t.partialEval(bs_)
    case (false :: bs_, Node(_, f)) => f.partialEval(bs_)
    case _ => None
  }

  /** Creates a list of Booleans for the calling CBDD object. The list is constructed by traversing the tree. For
    * each non-Terminal Node, an element is added to the list. If set evaluates to False, false is added
    * and a recursive call on the unset branch is made, else element true is added and a recursive call on the set
    * branch is made. Recursion stops once a Terminal is found. If the Terminal is True, the list is returned. If the
    * Terminal is False, the list is discarded and None is returned.
    * Returns the path to the leftmost True Terminal if one exists, None else.
    *
    * Analogue: falseMost.
    *
    * @return a path to the leftmost True Terminal
    */
  def trueMost: Option[List[Boolean]] = this match {
    case False => None
    case True => Some(Nil)
    case Node(False, uset) => uset.trueMost.map(false :: _)
    case Node(set, _) => set.trueMost.map(true :: _)
  }

  /** Creates a list of Booleans for the calling CBDD object. The list is constructed by traversing the tree. For
    * each non-Terminal Node, an element is added to the list. If unset evaluates to False, true is added
    * and a recursive call on the set branch is made, else element false is added and a recursive call on the unset
    * branch is made. Recursion stops once a Terminal is found. If the Terminal is True, the list is returned. If the
    * Terminal is False, the list is discarded and None is returned.
    * Returns the path to the rightmost True Terminal if one exists, None else.
    *
    * Analogue: trueMost.
    *
    * @return a path to the rightmost True Terminal
    */
  def falseMost: Option[List[Boolean]] = this match {
    case False => None
    case True => Some(Nil)
    case Node(set, False) => set.falseMost.map(true :: _)
    case Node(_, uset) => uset.falseMost.map(false :: _)
  }

  private def computeTruePaths(path: List[Boolean]): Stream[List[Boolean]] = this match {
    case False => Stream()
    case True => Stream(path)
    case Node(set, uset) => set.computeTruePaths(true :: path) #::: uset.computeTruePaths(false :: path)
  }

  /** Computes paths to True Terminals of calling CBDD object. Paths are represented as lists of Booleans, where
    * each subsequent Boolean value specifies if left (set) or right (unset) path is taken. Head to tail in list
    * represents top to bottom in tree. Paths are stored and returned in a Stream object.
    *
    * @return Stream object containing lists of paths to True Terminals
    */
  def truePaths = computeTruePaths(List()).map(_.reverse)

  /** Computes a random path to a True Terminal of calling CBDD object. Returns Nil if caller is True. Throws
    * NoSuchElementException if caller is False (i.e. an empty tree). Path is represented as list of Booleans,
    * where each subsequent Boolean value specifies if left (set) or right (unset) path is taken. Head to tail in
    * list represents top to bottom in tree.
    *
    * @return list of Booleans representing path to True Terminal
    */
  def randomTruePath() = {
    def helper(bdd: CBDD, path: List[Boolean]): List[Boolean] = bdd match {
      case True => path
      case False => throw new NoSuchElementException("randomTruePath on empty CBDD")
      case Node(False, uset) => helper(uset, false :: path)
      case Node(set, False) => helper(set, true :: path)
      case Node(set, uset) => {
        val dir = scala.util.Random.nextBoolean()
        if (dir) helper(set, true :: path) else helper(uset, false :: path)
      }
    }
    helper(this, Nil).reverse
  }

  /** Logical IMPLIES for CBDDs. Recursively traverses both argument CBDDs until a Terminal is found in either tree,
    * applies traditional IMPLIES on respective Nodes, conjuncts results. */
  def doesImply(that: CBDD): Boolean = (this, that) match {
    case (a, b) if a == b => true
    case (Node(set1, uset1), Node(set2, uset2)) => set1.doesImply(set2) && uset1.doesImply(uset2)
    case (False, False) => true
    case (True, True) => true
    case (_, True) => true
    case (False, _) => true
    case _ => false
  }

  /** Truncates the calling CBDD object at specified depth, cut subtree is set to True. Returns calling object if
    * tree depth is larger than specified depth.
    *
    * @param toTake depth, at which CBDD is to be cut
    * @return truncated CBDD object
    */
  def take(toTake: Int): CBDD = this match {
    case False => False
    case _ if toTake <= 0 => True
    case Node(set, uset) => Node(set.take(toTake - 1), uset.take(toTake - 1))
    case True => True
  }

  /** Drops part of the tree of the calling CBDD object by iteratively applying a reduction function on each Node,
    * applying a separate mapping function on Terminals or Nodes at specified depth (up to which to drop to).
    *
    * @param toDrop  depth, up to which to drop
    * @param acc     accumulator (TODO: WHAT'S THIS?)
    * @param mapF    mapping function to be applied to Terminals and Nodes at specified depth
    * @param reduceF function with which to reduce when dropping
    * @tparam A type parameter
    * @return the CBDD object resulting
    */
  def drop[A](toDrop: Int, acc: A, mapF: CBDD => A, reduceF: (A, A) => A): A = this match {
    case False => mapF(False)
    case _ if toDrop <= 0 => mapF(this)
    case Node(set, uset) => {
      val setDropped = set.drop(toDrop - 1, acc, mapF, reduceF)
      val usetDropped = uset.drop(toDrop - 1, acc, mapF, reduceF)
      reduceF(setDropped, usetDropped)
    }
    case True => mapF(True)
  }

  /** Particular application of drop, mapping function is identity, reduction function is OR.
    *
    * See: CBDD.drop
    */
  def dropOr(toDrop: Int) = this.drop(toDrop, False, (x: CBDD) => x, (a: CBDD, b: CBDD) => a || b)

  /** In calling CBDD, replaces any occurrences of one CBDD with another. Recursively checks calling object's tree for
    * matches of tree to replace. Returns CBDD where all occurrences have been replaced.
    *
    * @param toReplace     CBDD to replace
    * @param toReplaceWith CBDD to replace occurrences with
    * @return the resulting CBDD
    */
  def replaceWith(toReplace: CBDD, toReplaceWith: CBDD): CBDD = if (this == toReplace) toReplaceWith
  else this match {
    case True => True
    case False => False
    case Node(set, uset) if set == uset => {
      val sub = set.replaceWith(toReplace, toReplaceWith)
      Node(sub, sub)
    }
    case Node(set, uset) => Node(set.replaceWith(toReplace, toReplaceWith), uset.replaceWith(toReplace, toReplaceWith))
  }

  /** Naive widening. Given a precision Integer number, traverses two BDDs, checking where they are different. If
    * precision depth is reached, subtrees that are different are approximated as True.
    *
    * @param that      counterpart to this; the other BDD
    * @param precision integer, specifying the depth at which approximation should be done
    * @return the widened BDD
    */
  def widen_naive(that: CBDD, precision: Int): CBDD = {
    Console.println("widening_naive called, precision: " + precision)
    def helper(a: CBDD, b: CBDD, precision: Int): CBDD = {
      if (precision == 0) return True // depth is reached, subtree is set to True
      if (b == True || b == False || a == b || a == True) return b // return b if terminal or unchanged or a geq b
      (a, b) match {
        case (False, Node(bLeft, bRight)) =>
          Node(helper(False, bLeft, precision - 1), helper(False, bRight, precision - 1))
        case (Node(aLeft, aRight), Node(bLeft, bRight)) =>
          Node(helper(aLeft, bLeft, precision - 1), helper(aRight, bRight, precision - 1))
      }
    }
    helper(this, that, precision)
  }

  /** Similar to naive approach. Argument BDD has to be strictly greater equal caller BDD.
    *
    * @param that
    * @param precision
    * @return
    */
  def widen_naive_hasStrictlyGrown(that: CBDD, precision: Int): CBDD = {
    Console.println("widen_naive_hasStrictlyGrown called, precision: " + precision)
    def helper(a: CBDD, b: CBDD, precision: Int): CBDD = {
      if (precision == 0){
        if (a.doesImply(b) && !a.equals(b)) {
          return True
        }
        else return b
      } // depth is reached, subtree is set to True
      if (b == True || b == False || a == b || a == True) return b // return b if terminal or unchanged or a geq b
      (a, b) match {
        case (False, Node(bLeft, bRight)) =>
          Node(helper(False, bLeft, precision - 1), helper(False, bRight, precision - 1))
        case (Node(aLeft, aRight), Node(bLeft, bRight)) =>
          Node(helper(aLeft, bLeft, precision - 1), helper(aRight, bRight, precision - 1))
      }
    }
    helper(this, that, precision)
  }

  override def toString = bdd.toString(compl)

  /** Logical EQUALS for CBDDs. */
  override def equals(that: Any) = that match {
    case t: CBDD => compl == t.compl && (bdd eq t.bdd)
    case _ => false
  }

  override def hashCode = (bdd.hashCode, compl).hashCode
}

/** CBDD object.
  */
object CBDD {

  /** Iteratively constructs a CBDD from a nonempty list of Booleans. (TODO: = PATH?)
    * If head is true, a Node is constructed where set is the tree constructed from the remaining list, unset is False,
    * If head is false, set and unset are swapped. If no bits remain, True is added instead.
    *
    * @param bits bitwise construction imperatives
    * @return the constructed Node object representing a CBDD.
    */
  def apply(bits: List[Boolean]): CBDD = {
    require(bits match { case Nil => false; case _ => true })
    def helper(hbits: List[Boolean]): CBDD = hbits match {
      case true :: remBits => Node(helper(remBits), False)
      case false :: remBits => Node(False, helper(remBits))
      case Nil => True
    }
    helper(bits)
  }

  /** Iteratively constructs a CBDD from a path to a terminal represented as a list of Booleans, two CBDD subtrees,
    * and a terminal CBDD. If the path is empty, terminal is returned. If head is true, a Node is
    * constructed where set is the tree constructed from the remaining list, unset is uset argument.
    * If head is false, set and unset are swapped.
    *
    * @param path     the path to the terminal Node, represented as a list of Booleans
    * @param set      set Node subtree
    * @param uset     uset Node subtree
    * @param terminal terminal Node
    * @return the Node representing the constructed CBDD
    */
  def apply(path: List[Boolean], set: CBDD, uset: CBDD, terminal: CBDD): CBDD = path match {
    case Nil => terminal
    case true :: path_ => Node(CBDD(path_, set, uset, terminal), uset)
    case false :: path_ => Node(set, CBDD(path_, set, uset, terminal))
  }

  /** Iteratively constructs a CBDD from two paths to XXX represented as lists of Booleans. TODO do ite first
    *
    * @param path1
    * @param path2
    * @return
    */
  def apply(path1: List[Boolean], path2: List[Boolean]): CBDD = {
    def checker(p1: List[Boolean], p2: List[Boolean]): Boolean = (p1, p2) match {
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

  private def pred(integer: List[Boolean]): List[Boolean] = {
    def helper(bs: List[Boolean]): (Boolean, List[Boolean]) = bs match {
      case List(x) => (x, List(!x))
      case b :: bs => {
        val (flipped, rec) = helper(bs)
        (b, (b == flipped) :: rec)
      }
    }
    helper(integer)._2
  }

  /** Method implementing insertion.
    *
    * @param op1
    * @param op2
    * @param depth
    * @return
    */
  def plus(op1: CBDD, op2: CBDD, depth: Int): CBDDTuple = (op1, op2) match {
    case (False, _) => (False, False)
    case (x, False) => plus(False, x, depth)
    case (True, True) => {
      val ov = if (depth == 0) False else !CBDD(List.fill(depth)(true))
      (True, ov)
    }
    case (n@Node(set, uset), True) => {
      val lo = n.falseMost.get.padTo(depth, false)
      val hi = n.trueMost.get.padTo(depth, true)
      val noOv = CBDD(lo, List.fill(depth)(true))
      val ov = if (lo.forall(!_)) False else CBDD(List.fill(depth)(false), pred(hi))
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

  /** Method implementing negation.
    *
    * @param bits
    * @param bdd
    * @return
    */
  def negate(bits: Int, bdd: CBDD) = {
    val (ov, norm) = plus(bNot(bdd), (CBDD(List.fill(bits - 1)(false) ++ List(true))), bits)
    ov || norm
  }

  private def bitwiseOpHelper(op: (Boolean, Boolean) => Boolean)(trueFalseTuples: List[((CBDD, Boolean), (CBDD, Boolean))]): CBDD = {
    val trueFalse = trueFalseTuples.distinct.partition((t) => op(t._1._2, t._2._2))
    val nset = ((False: CBDD) /: trueFalse._1) ((acc, t) => acc || bitwiseOp(op)(t._1._1, t._2._1))
    val nuset = ((False: CBDD) /: trueFalse._2) ((acc, t) => acc || bitwiseOp(op)(t._1._1, t._2._1))
    Node(nset, nuset)
  }

  /** Method
    *
    * @param op
    * @param op1
    * @param op2
    * @return
    */
  def bitwiseOp(op: (Boolean, Boolean) => Boolean)(op1: CBDD, op2: CBDD): CBDD = (op1, op2) match {
    case (False, _) => False
    case (x, False) => bitwiseOp(op)(False, x)
    case (True, True) => True
    case (Node(set, uset), True) => {
      val trueFalseTuples = List(((set, true), (True, true)), ((set, true), (True, false)), ((uset, false), (True, true)), ((uset, false), (True, false)))
      bitwiseOpHelper(op)(trueFalseTuples)
    }
    case (True, Node(set, uset)) => {
      //list this case explicitly to not force op to be commutative
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
        case True => True
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
      val ft = if (set1 == uset1) tt else bAnd(uset1, set2)
      val ff = if (set2 == uset2) ft else bAnd(uset1, uset2)
      val tf = if (set1 == uset1) ff else if (set2 == uset2) tt else bAnd(set1, uset2)
      val nuset = union3(tf, ft, ff)
      Node(tt, nuset)
    }
  }

  def bOr(op1: CBDD, op2: CBDD): CBDD = (op1, op2) match {
    case (False, _) => False
    case (_, False) => bOr(op2, op1)
    case (True, x) => {
      def trueRecurse(bdd: CBDD): CBDD = bdd match {
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
      val ft = if (set1 == uset1) tt else bOr(uset1, set2)
      val ff = if (set2 == uset2) ft else bOr(uset1, uset2)
      val tf = if (set1 == uset1) ff else if (set2 == uset2) tt else bOr(set1, uset2)
      val nset = union3(tt, ft, tf)
      Node(nset, ff)
    }
  }

  def bXOr(op1: CBDD, op2: CBDD): CBDD = (op1, op2) match {
    case (False, _) => False
    case (_, False) => bXOr(op2, op1)
    case (True, _) => True
    case (_, True) => bXOr(op2, op1)
    case (Node(set1, uset1), Node(set2, uset2)) => {
      val tt = bXOr(set1, set2)
      val ft = if (set1 == uset1) tt else bXOr(uset1, set2)
      val ff = if (set2 == uset2) ft else bXOr(uset1, uset2)
      val tf = if (set1 == uset1) ff else if (set2 == uset2) tt else bXOr(set1, uset2)
      val nset = ft || tf
      val nuset = tt || ff
      Node(nset, nuset)
    }
  }

  def bNot(op1: CBDD): CBDD = op1 match {
    case False => False
    case True => True
    case Node(set, uset) if set == uset => {
      val not = bNot(set)
      Node(not, not)
    }
    case Node(set, uset) => Node(bNot(uset), bNot(set))
  }
}

/** Class of iterator over CBDD.
  * Provides methods hasNext, next.
  *
  * @param cbdd   CBDD to iterate over
  * @param layers TODO
  */
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
        wl = (set, true :: bs) ::(uset, false :: bs) :: wl_
        computeNext()
      }
    }
  }

  /** Computes if iterator has another object to go to. */
  def hasNext(): Boolean = nextElem match {
    case None => {
      computeNext()
      nextElem match {
        case None => false
        case Some(_) => hasNext()
      }
    }
    case Some(_) => true
  }

  /** Computes the next object of the iterator. */
  def next(): List[Boolean] = nextElem match {
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
    case Nil => None
    case false :: bs1 => Some(true :: bs1)
    case true :: bs1 => succList(bs1) match {
      case None => None
      case Some(bs2) => Some(false :: bs2)
    }
  }
}

/** Terminal object for CBDDs.
  * BDD value fields are set accordingly.
  * Provides methods toString, hashCode.
  */
object Terminal extends BDD {
  val depth: Int = 0
  val tag: Int = 0
  val count: Long = 1
  val nodecount: Long = 1
  val compl: Boolean = false

  def toString(c: Boolean) = if (c) "False(" + tag + ", " + depth + ", " + 0 + ")" else "True(" + tag + ", " + depth + ", " + 1 + ")"

  override def hashCode = tag
}

/** Terminal node object of CBDDs holding "true". Representative of all "true" terminals.
  * Provides extractor method, returning its argument if argument is terminal and not complemented, else None.
  */
object True extends CBDD(Terminal, false) {
  def unapply(cbdd: CBDD) = if (cbdd.bdd == Terminal && !cbdd.compl) Some(cbdd) else None
}

/** Terminal node object of CBDDs holding "false". Representative of all "false" terminals.
  * Provides extractor method, returning its argument if argument is terminal and complemented, else None.
  */
object False extends CBDD(Terminal, true) {
  def unapply(cbdd: CBDD) = if (cbdd.bdd == Terminal && cbdd.compl) Some(cbdd) else None
}

/** CBDD node object.
  * Uses a private WeakHashMap as cache, where for each BDD key a WeakReference to it is the value.
  * Provides method cacheSize, returning size of cache.
  * Provides method nodeCount, returning a tuple where left is the number of nodes in cache which were counted,
  * right is true if some BDD was very large and its nodes not counted, false if all nodes were counted.
  * Provides method status, which returns a string holding information on cache status.
  */
object Node {
  private[this] val cache = WeakHashMap.empty[BDD, WeakReference[BDD]]
  private[this] var tagCounter: Int = 1

  def cacheSize() = cache.size

  /** Counts nodes in cache, returning a tuple. Iterates over cache, in right summing up amount of nodes in BDDs.
    * If node count is very large in any BDD, ignores corresponding entry and saves information if any were skipped as
    * Boolean in right.
    *
    * @return TODO
    */
  def nodeCount() = ((0: BigInt, false) /: cache.toList) { case ((n, huge), (bdd, _)) =>
    if (bdd.nodecount == -1) (n, true) else (unsignedLongToBigInt(bdd.nodecount) + n, huge)
  }

  def status(): String = {
    val nc = nodeCount()
    "Tag counter: " + tagCounter + "; Items in cache: " + cacheSize() + (if (cacheSize() != 0) "; Nodes in cache: " + nc._1 + "; Average BDD size: " + (nc._1 / cacheSize()) + (if (nc._2) "; There where huge uncounted BDDs" else "") else "")
  }

  /** Constructs a BDD from two subtrees, returning either a new CBDD or the reference in the cache if an equivalent
    * object already exists. Updates cache if necessary.
    *
    * @param set  set subtree - BDD for true edge
    * @param uset unset subtree - BDD for false edge
    * @return CBDD object constructed of input CBDDs
    */
  def apply(set: CBDD, uset: CBDD) = {
    val ibit = set.compl
    if (set.compl == uset.compl && set.bdd == Terminal && uset.bdd == Terminal) new CBDD(Terminal, ibit)
    else {
      val usetbit = set.compl != uset.compl
      val tentative = new Node(set.bdd, uset.bdd, usetbit, tagCounter)
      val hashconsed = cache.getOrElseUpdate(tentative, WeakReference(tentative)).get.getOrElse(tentative)
      if (tentative eq hashconsed) tagCounter += 1;
      new CBDD(hashconsed, ibit)
    }
  }

  /** Extracts child trees from CBDD.
    *
    * @param cbdd parent CBDD
    * @return tuple of child CBDDs paired with respective complement information
    */
  def unapply(cbdd: CBDD) = cbdd.bdd match {
    case b: Node => {
      val setbit = cbdd.compl
      val usetbit = cbdd.compl != b.compl
      Some((new CBDD(b.set, setbit), new CBDD(b.uset, usetbit)))
    }
    case _ => None
  }
}

/** Class of CBDD nodes. Instantiated with two BDDs for true and false edges, complement bit, tag information.
  * Value depth provides information on total depth of subtree.
  * Value count provides information on total number of paths to terminal nodes of True.
  * Value nodecount provides information on total count of nodes in subtree.
  * Provides methods toString, hashCode, equals.
  *
  * @param set   set subtree - BDD for true edge
  * @param uset  unset subtree - BDD for false edge
  * @param compl complement bit - information if node is complemented
  * @param tag   tag integer
  */
final class Node(val set: BDD, val uset: BDD, val compl: Boolean, val tag: Int) extends BDD {
  val depth = scala.math.max(set.depth, uset.depth) + 1

  private def countR: Long = uset match {
    case Terminal if (compl) => 0
    case Terminal => 1
    // if unset edge is complemented (maximum possible number of nodes in complete subtree - recursive call of count on subtree)
    // else (recursive call of count on subtree)
    case _ => if (compl) (1l << uset.depth) - uset.count else uset.count
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
    case _ => false
  }

  val nodecount = {
    val res = set.nodecount + uset.nodecount
    //-1 indicates that overflow happened over 0 (overflow in unsigned long)
    if (set.nodecount >= 0 && uset.nodecount >= 0 || res < 0) res else -1
  }
}