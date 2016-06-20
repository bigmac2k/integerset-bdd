package cc.felix

import cc.sven.bdd._

/** ASSUME WELL FORMED INPUT,
  * ASSUME ONE VARIABLE NOT MULTIPLE TIMES IN SAME CONJUNCTION
  *
  * @param s
  * @param v
  */
class LExp(s: List[LLit], v: List[Char]) {
  var symbols: List[LLit] = s
  var vars: List[Char] = v

  def toBDD: CBDD = {
    // separate into conjunctions
    def splitAtConj(symbols: List[LLit], acc: List[LLit]): List[List[LLit]] = symbols match {
      case Nil => List(acc.reverse)
      case a :: Nil => splitAtConj(Nil, a :: acc)
      case h :: t if h == LOr => acc.reverse :: splitAtConj(t, Nil)
      case h :: t => splitAtConj(t, h :: acc)
    }
    val conjs: List[List[LLit]] = splitAtConj(symbols, Nil)
    // compute each conjunction
    var conjBDDs: List[CBDD] = Nil

    /** Create Node from conjunction */
    def createFromConj(conj: List[LLit]): CBDD = {
      val rv: List[Char] = vars.reverse
      var charCBDDs: List[CBDD] = Nil
      var found: Boolean = false
      for (i <- 0 to rv.length - 1) {
        // for all variables
        for (cLit <- conj) {
          // for each literal in conjunction
          if (cLit.isInstanceOf[LAlphaVar]) {
            // if this is a variable literal
            if (cLit.getC == rv(i)) {
              // if it's the right one
              // char is in conj: false to false, true to next lower
              found = true
              if (charCBDDs == Nil) {
                // char is lowest, create elementary TODO: INVERTED VARIABLES
                charCBDDs = Node(True, False) :: charCBDDs
              } else {
                // char has lower, false to false, true to next lower
                charCBDDs = Node(charCBDDs.head, False) :: charCBDDs
              }
            }
          }
        }
        if (!found) {
          // char is  NOT in conj: both to lower or false
          if (charCBDDs == Nil) {
            // char is lowest, create elementary
            charCBDDs = True :: charCBDDs
          } else {
            // char has lower, both to lower
            charCBDDs = Node(charCBDDs.head, charCBDDs.head) :: charCBDDs
          }
        }
        found = false
      }
      charCBDDs.head
    }

    for (conj <- conjs) {
      // for every conjunction, create BDD
      conjBDDs = createFromConj(conj) :: conjBDDs
    }

    def unify(conjBDDs: List[CBDD]): CBDD = conjBDDs match {
      case Nil => False
      case h :: t => h || unify(t)
    }
    unify(conjBDDs)

  }

  def eval = {
    def evalNeg(in: List[LLit], out: List[LLit]): List[LLit] = in match {
      case List() => out.reverse
      case LNeg :: LNeg :: x => x
      case LNeg :: a :: _ => {
        val r = LNeg(a)
        if (r.symbols.length > 1)
          evalNeg(in.drop(1), r.symbols.head :: out)
        else {
          if (out.length < 1)
            evalNeg(r.symbols ::: in.drop(2), out)
          else
            evalNeg(out.head :: r.symbols ::: in.drop(2), out.tail)
        }
      }
      case _ => evalNeg(in.tail, in.head :: out)
    }
    symbols = evalNeg(symbols, Nil)

    def evalAnd(in: List[LLit], out: List[LLit]): List[LLit] = in match {
      case List() => out.reverse
      case a :: LAnd :: b :: _ => {
        val r = LAnd(a, b)
        if (r.symbols.length > 1)
          evalAnd(in.drop(2), r.symbols.take(2).reverse ::: out)
        else
          evalAnd(out.take(2).reverse ::: r.symbols ::: in.drop(3), out.drop(2))
      }
      case _ => evalAnd(in.tail, in.head :: out)
    }
    symbols = evalAnd(symbols, Nil)

    def evalOr(in: List[LLit], out: List[LLit]): List[LLit] = in match {
      case List() => out.reverse
      case a :: LOr :: b :: _ => {
        val r = LOr(a, b)
        if (r.symbols.length > 1)
          evalOr(in.drop(2), r.symbols.take(2).reverse ::: out)
        else
          evalOr(out.take(2).reverse ::: r.symbols ::: in.drop(3), out.drop(2))
      }
      case _ => evalOr(in.tail, in.head :: out)
    }
    symbols = evalOr(symbols, Nil)
  }

  def show = {
    symbols.foreach(print)
    println("")
  }
}

object LExp {

  def apply(s: String): LExp = {
    var v: List[Char] = Nil
    def stringToList(s: String): List[LLit] = {
      def helper(s: String, acc: List[LLit]): List[LLit] = s match {
        case "" => acc.reverse
        case s if s startsWith "1" => helper(s.tail, LTrue :: acc)
        case s if s startsWith "0" => helper(s.tail, LFalse :: acc)
        case s if s startsWith "*" => helper(s.tail, LAnd :: acc)
        case s if s startsWith "+" => helper(s.tail, LOr :: acc)
        case s if s startsWith "!" => helper(s.tail, LNeg :: acc)
        case s if s.charAt(0) >= 'a' && s.charAt(0) <= 'z' =>
          v = insert(v, s.charAt(0))
          helper(s.drop(1), new LAlphaVar(s.charAt(0)) :: acc)
        case _ => throw new Exception("can not construct LExp from string: " + s)
      }

      helper(s.replaceAll("\\s", ""), Nil)
    }
    val r = new LExp(stringToList(s), v)
    r
  }

  def insert(v: List[Char], c: Char): List[Char] = {
    def helper(v: List[Char], c: Char): List[Char] = v match {
      case Nil => List(c)
      case h :: _ if h == c => v
      case h :: t if h > c => c :: v
      case h :: t if h < c => h :: helper(t, c)
    }
    helper(v, c)
  }

  def apply(s: List[LLit], v: List[Char]): LExp = {
    val a = new LExp(s, v)
    a
  }

  def apply(s: LLit): LExp = {
    val a = new LExp(List(s), Nil)
    a
  }
}

class LLit() {
  // this a hack
  var isInv: Boolean = false
  var c: Char = '-'

  def negate = {
    if (!this.isInstanceOf[LAlphaVar]) throw new Exception("trying to negate non-variable literal")
    else
      isInv = !isInv
  }

  def getC: Char = {
    if (!this.isInstanceOf[LAlphaVar]) throw new Exception("trying to get character from non-variable literal")
    else
      c
  }
}

object LTrue extends LLit {

  override def toString = "1"
}

object LFalse extends LLit {

  override def toString = "0"
}

object LAnd extends LLit {
  def apply(a: LLit, b: LLit): LExp = (a, b) match {
    case (LFalse, b) => LExp(LFalse)
    case (a, LFalse) => LExp(LFalse)
    case (LTrue, b) => LExp(b)
    case (a, LTrue) => LExp(a)
    case (a, b) if (a == b) => LExp(a) // here
    case x => LExp(List(a, LAnd, b), Nil)
  }

  override def toString = "*"
}

object LOr extends LLit {
  def apply(a: LLit, b: LLit): LExp = (a, b) match {
    case (LTrue, _) => LExp(LTrue)
    case (_, LTrue) => LExp(LTrue)
    case (a, LFalse) => LExp(a)
    case (LFalse, b) => LExp(b)
    case (a, b) if (a == b) => LExp(a) // here
    case _ => LExp(List(a, LOr, b), Nil)
  }

  override def toString = "+"
}

object LNeg extends LLit() {
  def apply(a: LLit): LExp = a match {
    case LTrue => LExp(LFalse)
    case LFalse => LExp(LTrue)
    case LNeg => LExp(Nil, Nil)
    case x if x.isInstanceOf[LAlphaVar] => {
      a.negate
      LExp(a)
    }
    case _ => LExp(List(LNeg, a), Nil)
  }

  override def toString = "!"
}

object LAlphaVar extends LLit() {

}

class LAlphaVar(ch: Char) extends LLit {
  c = ch

  override def toString = if (isInv) "NOT(" + c + ")" else "" + c
}
