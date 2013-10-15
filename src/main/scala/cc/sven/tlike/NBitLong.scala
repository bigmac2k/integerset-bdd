package cc.sven.tlike

import cc.sven.bounded._

class NBitLong(val bits : Int, val value : Long) {
  import scala.math.BigInt.int2bigInt
  require(value < (2 pow bits))
  override def equals(that : Any) = that match {
    case that : NBitLong => bits == that.bits && value == that.value
    case _ => false
  }
  override def hashCode = value.intValue
  override def toString = {
    "|" + bits.toString + ":" + NBitLong.signExtend(bits, value).toString + "|"
  }
}
object NBitLong {
  def signExtend(bits : Int, value : Long) : Long = {
    val longBits = implicitly[BoundedBits[Long]].bits
    val signMask = 1l << (bits - 1)
    val extendedValue = if((signMask & value) == 0) value else {
      val extendMask : Long = (1l /: List.range(bits, longBits))((acc, p) => acc | (1l << p))
      value | extendMask
    }
    extendedValue
  }
  def signContract(bits : Int, value : Long) : Long = {
    val longBits = implicitly[BoundedBits[Long]].bits
    val mask : Long = (1l /: List.range(0, bits))((acc, p) => acc | (1l << p))
    value & mask
  }
  def apply(bits : Int, value : Long) = {
    val min = signExtend(bits, (1l << bits - 1))
    val max = signContract(bits - 1, -1)
    val longBits = implicitly[BoundedBits[Long]].bits
    //println("bits: " + bits, "min: " + min, "max: " + max, "value: " + value)
    require(bits <= longBits && value <= max && value >= min)
    new NBitLong(bits, signContract(bits, value))
  }
  def apply(value : Long) : NBitLong = {
    val longBits = implicitly[BoundedBits[Long]].bits
    apply(longBits, value)
  }
  class NBitLongIsOrderedC extends Ordering[NBitLong] {
    def compare(a : NBitLong, b : NBitLong) : Int = implicitly[Ordering[Long]].compare(a.value, b.value)
  }
  implicit val NBitLongIsOrdered = new NBitLongIsOrderedC
  implicit object NBitLongIsDynBoundedBits extends DynBoundedBits[NBitLong] {
    def dBits(nbl : NBitLong) = nbl.bits
  }
  implicit object NBitLongIsDynBounded extends DynBounded[NBitLong] {
    import scala.math.BigInt.int2bigInt
    def dMaxBound(nbl : NBitLong) = apply(nbl.bits, (2 pow (nbl.bits - 1) - 1).longValue)
    def dMinBound(nbl : NBitLong) = apply(nbl.bits, (2 pow (nbl.bits - 1)).longValue)
  }
  implicit object NBitLongIsIntegral extends NBitLongIsOrderedC with Integral[NBitLong] {
    def plus(x : NBitLong, y : NBitLong) : NBitLong = {
      require(x.bits == y.bits)
      apply(x.bits, x.value + y.value)
    }
    def minus(x : NBitLong, y : NBitLong) : NBitLong = {
      require(x.bits == y.bits)
      apply(x.bits, x.value - y.value)
    }
    def times(x : NBitLong, y : NBitLong) : NBitLong = {
      require(x.bits == y.bits)
      apply(x.bits, x.value * y.value)
    }
    def quot(x : NBitLong, y : NBitLong) : NBitLong = {
      require(x.bits == y.bits)
      apply(x.bits, x.value / y.value)
    }
    def rem(x : NBitLong, y : NBitLong) : NBitLong = {
      require(x.bits == y.bits)
      apply(x.bits, x.value % y.value)
    }
    def negate(x : NBitLong) : NBitLong = apply(x.bits, -x.value)
    def fromInt(x : Int) : NBitLong = {
      val boundedBits = implicitly[BoundedBits[Int]]
      apply(boundedBits.bits, x.toLong)
    }
    def toInt(x : NBitLong) : Int = x.value.toInt
    def toLong(x : NBitLong) : Long = x.value
    def toFloat(x : NBitLong) : Float = x.value.toFloat
    def toDouble(x : NBitLong) : Double = x.value.toDouble
  }
  implicit object NBitLongIsLongCastable extends Castable[NBitLong, (Int, Long)] {
    def apply(x : NBitLong) : (Int, Long) = (x.bits, x.value)
  }
  implicit object NBitLongIsNBitLongCastable extends Castable[(Int, Long), NBitLong] {
    def apply(x : (Int, Long)) : NBitLong = new NBitLong(x._1, x._2)
  }
}