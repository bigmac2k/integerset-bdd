package cc.felix

//import cc.sven.bdd

class LExp(s: List[LSym]) {
  var symbols: List[LSym] = s

  def eval = {
    def evalNeg(in: List[LSym], out: List[LSym]): List[LSym] = in match {
      case List() => out.reverse
      case LNeg :: a :: _ => {
        val r = LNeg(a)
        if (r.symbols.length > 1) evalNeg(in.drop(2), r.symbols.reverse ::: out) else evalNeg(r.symbols ::: in.drop(2),
          out)
      }
      case _ => evalNeg(in.tail, in.head :: out)
    }
    symbols = evalNeg(symbols, Nil)

    def evalAnd(in: List[LSym], out: List[LSym]): List[LSym] = in match {
      case List() => out.reverse
      case a :: LAnd :: b :: _ => {
        val r = LAnd(a, b)
        if (r.symbols.length > 1)
          evalAnd(in.drop(3), r.symbols.reverse ::: out)
        else
          evalAnd(r.symbols ::: in.drop(3), out)
      }
      case _ => evalAnd(in.tail, in.head :: out)
    }
    symbols = evalAnd(symbols, Nil)

    def evalOr(in: List[LSym], out: List[LSym]): List[LSym] = in match {
      case List() => out.reverse
      case a :: LOr :: b :: _ => {
        val r = LOr(a, b)
        if (r.symbols.length > 1) evalOr(in.drop(3), r.symbols.reverse ::: out)
        else evalOr(r.symbols ::: in.drop(3),
          out)
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
    def stringToList(s: String): List[LSym] = {
      def helper(s: String, acc: List[LSym]): List[LSym] = s match {
        case "" => acc.reverse
        case s if s startsWith ("t") => helper(s.tail, LTrue :: acc)
        case s if s startsWith ("f") => helper(s.tail, LFalse :: acc)
        case s if s startsWith ("*") => helper(s.tail, LAnd :: acc)
        case s if s startsWith ("+") => helper(s.tail, LOr :: acc)
        case s if s startsWith ("!") => helper(s.tail, LNeg :: acc)
        // case s if s startsWith ("(") => helper(s.drop(1), LLB :: acc)
        // case s if s startsWith (")") => helper(s.drop(1), LRB :: acc)
        case s if s.charAt(0) >= 'a' && s.charAt(0) <= 'z' => helper(s.drop(1), new LAlphaVar(s.charAt(0)) :: acc)
        case _ => throw new Exception()
      }

      helper(s.replaceAll("\\s", ""), Nil)
    }
    LExp(stringToList(s))
  }

  def apply(s: List[LSym]): LExp = {
    val a = new LExp(s)
    a
  }

  def apply(s: LSym): LExp = {
    val a = new LExp(List(s))
    a
  }
}

class LSym() {

}

object LTrue extends LSym {

  override def toString = "1"
}

object LFalse extends LSym {

  override def toString = "0"
}

object LAnd extends LSym {
  def apply(a: LSym, b: LSym): LExp = (a, b) match {
    case (LTrue, b) => LExp(b)
    case (a, LTrue) => LExp(a)
    case (LFalse, b) => LExp(LFalse)
    case (a, LFalse) => LExp(LFalse)
    case x => LExp(List(a, LAnd, b))
  }

  override def toString = "*"
}

object LOr extends LSym {
  def apply(a: LSym, b: LSym): LExp = (a, b) match {
    case (LTrue, _) => LExp(LTrue)
    case (_, LTrue) => LExp(LTrue)
    case (a, LFalse) => LExp(a)
    case (LFalse, b) => LExp(b)
    case _ => LExp(List(a, LOr, b))
  }

  override def toString = "+"
}

object LNeg extends LSym() {
  def apply(a: LSym): LExp = a match {
    case LTrue => LExp(LFalse)
    case LFalse => LExp(LTrue)
    case _ => LExp(List(LNeg, a))
  }

  override def toString = "!"

}

//object LLB extends LSym {
//}

//object LRB extends LSym {
//}

class LAlphaVar(c: Char) extends LSym {

  override def toString = "" + c
}