package cc.felix

import cc.sven.bdd._


class LExp(s: List[LLit]) {
  var symbols: List[LLit] = s

  def makeBDD = {
    Node(True, False)
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
    def stringToList(s: String): List[LLit] = {
      def helper(s: String, acc: List[LLit]): List[LLit] = s match {
        case "" => acc.reverse
        case s if s startsWith "1" => helper(s.tail, LTrue :: acc)
        case s if s startsWith "0" => helper(s.tail, LFalse :: acc)
        case s if s startsWith "*" => helper(s.tail, LAnd :: acc)
        case s if s startsWith "+" => helper(s.tail, LOr :: acc)
        case s if s startsWith "!" => helper(s.tail, LNeg :: acc)
        case s if s.charAt(0) >= 'a' && s.charAt(0) <= 'z' => helper(s.drop(1), new LAlphaVar(s.charAt(0)) :: acc)
        case _ => throw new Exception("can not construct LExp from string: " + s)
      }

      helper(s.replaceAll("\\s", ""), Nil)
    }
    LExp(stringToList(s))
  }

  def apply(s: List[LLit]): LExp = {
    val a = new LExp(s)
    a
  }

  def apply(s: LLit): LExp = {
    val a = new LExp(List(s))
    a
  }
}

class LLit() {
  // this a hack
  var isInv: Boolean = false

  def negate = {
    if (!this.isInstanceOf[LAlphaVar]) throw new Exception("trying to negate non-variable literal")
    else
      isInv = !isInv
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
    case (a, b) if(a == b) => LExp(a) // here
    case x => LExp(List(a, LAnd, b))
  }

  override def toString = "*"
}

object LOr extends LLit {
  def apply(a: LLit, b: LLit): LExp = (a, b) match {
    case (LTrue, _) => LExp(LTrue)
    case (_, LTrue) => LExp(LTrue)
    case (a, LFalse) => LExp(a)
    case (LFalse, b) => LExp(b)
    case (a, b) if(a == b) => LExp(a) // here
    case _ => LExp(List(a, LOr, b))
  }

  override def toString = "+"
}

object LNeg extends LLit() {
  def apply(a: LLit): LExp = a match {
    case LTrue => LExp(LFalse)
    case LFalse => LExp(LTrue)
    case LNeg => LExp(Nil)
    case x if x.isInstanceOf[LAlphaVar] => {
      a.negate
      LExp(a)
    }
    case _ => LExp(List(LNeg, a))
  }

  override def toString = "!"
}

object LAlphaVar extends LLit() {

}

class LAlphaVar(c: Char) extends LLit {

  override def toString = if (isInv) "NOT(" + c + ")" else "" + c
}
