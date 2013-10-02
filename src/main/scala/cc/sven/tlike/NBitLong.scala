package cc.sven.tlike

import cc.sven.bounded._

class NBitLong(val bits : Int, val value : Long) {
  import scala.math.BigInt.int2bigInt
  require(value < (2 pow bits))
  override def toString = {
    "|" + bits.toString + ":" + NBitLong.signExtend(bits, value).toString + "|"
  }
}
object NBitLong {
  def signExtend(bits : Int, value : Long) : Long = {
    val longBits = implicitly[BoundedBits[Long]].bits
    val signMask = 1 << (bits - 1)
    val extendedValue = if((signMask & value) == 0) value else {
      val extendMask : Long = ((0 : Long) /: List.range(bits, longBits))((acc, p) => acc | ((1 : Long) << p))
      value | extendMask
    }
    extendedValue
  }
  def signContract(bits : Int, value : Long) : Long = {
    val longBits = implicitly[BoundedBits[Long]].bits
    val mask : Long = ((0 : Long) /: List.range(0, bits))((acc, p) => acc | ((1 : Long) << p))
    value & mask
  }
  def apply(bits : Int, value : Long) = {
    val min = signExtend(bits, ((1 : Long) << bits - 1))
    val max = signContract(bits - 1, -1)
    println(min, max)
    val longBits = implicitly[BoundedBits[Long]].bits
    require(bits <= longBits && value <= max && value >= min)
    new NBitLong(bits, signContract(bits, value))
  }
  implicit object NBitLongIsOrdered extends Ordering[NBitLong] {
    def compare(a : NBitLong, b : NBitLong) : Int = implicitly[Ordering[Long]].compare(a.value, b.value)
  }
  implicit object NBitLongIsDynBoundedBits extends DynBoundedBits[NBitLong] {
    def dBits(nbl : NBitLong) = nbl.bits
  }
  implicit object NBitLongIsDynBounded extends DynBounded[NBitLong] {
    import scala.math.BigInt.int2bigInt
    def dMaxBound(nbl : NBitLong) = apply(nbl.bits, (2 pow (nbl.bits - 1) - 1).longValue)
    def dMinBound(nbl : NBitLong) = apply(nbl.bits, (2 pow (nbl.bits - 1)).longValue)
  }
  //TODO Integral
}