package cc.sven.intset

trait IntegerIsIntegral extends Integral[Integer] {
  def plus(x: Integer, y: Integer): Integer = x.intValue + y
  def minus(x: Integer, y: Integer): Integer = x.intValue - y
  def times(x: Integer, y: Integer): Integer = x.intValue * y
  def quot(x: Integer, y: Integer): Integer = x.intValue / y
  def rem(x: Integer, y: Integer): Integer = x.intValue % y
  def negate(x: Integer): Integer = -x.intValue
  def fromInt(x: Int): Integer = x
  def toInt(x: Integer): Int = x
  def toLong(x: Integer): Long = x.intValue
  def toFloat(x: Integer): Float = x.intValue.toFloat
  def toDouble(x: Integer): Double = x.intValue.toDouble
}
trait IntegerOrdering extends Ordering[Integer] {
  def compare(x: Integer, y: Integer): Int = x.intValue compare y.intValue
}

object IntegerIntegral {
  implicit val integerIsIntegral = new JIntegerIsIntegral
}