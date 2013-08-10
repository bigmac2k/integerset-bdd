package cc.sven.intset

import cc.sven.bdd._

class Ival[T](val lo : Numeric[T], val hi : Numeric[T]) {
  override def toString = "[" + lo.toString + " .. " + hi.toString + "]"
}
/*object Ival {
  def apply[T](lo : T, hi : T) : Ival[T] = new Ival(lo, hi)
  def unapply[T](ival : Ival[T]) = Some(ival.lo, ival.hi)
}*/
/*class StridedIval[T](lo : T, hi : T, val stride : T) extends Ival[T](lo, hi) {
  val next = lo + stride
  override def toString = "[" + lo.toString + ", " + lo.toString + " .. " + hi.toString + "]"
}*/

/*class StridedIval[T](lo : T, hi : T, val stride : T) extends Ival(lo, hi)

class IntSet /*extends BDDLike*/ {
  //enumeration
  //interval
  //strided ival
  
}*/