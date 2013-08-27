package cc.sven.bounded

object Bounded {
  implicit object IntIsBounded extends Bounded[Int] {
    val maxBound = Int.MaxValue
    val minBound = Int.MinValue
  }
  implicit object IntegerIsBounded extends Bounded[Integer] {
    val maxBound = new Integer(Integer.MAX_VALUE)
    val minBound = new Integer(Integer.MIN_VALUE)
  }
}

trait Bounded[T] {
  val maxBound: T
  val minBound: T
}

object BoundedBits {
  implicit object IntIsBoundedBits extends BoundedBits[Int] {
    val bits = 32
  }
  implicit object IntegerIsBoundedBits extends BoundedBits[Integer] {
    val bits = 32
  }
}

trait BoundedBits[T] {
  val bits: Int
}