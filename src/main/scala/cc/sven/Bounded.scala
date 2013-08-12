package cc.sven.bounded

object Bounded {

  implicit object IntIsBounded extends Bounded[Int] {
    val maxBound = Int.MaxValue
    val minBound = Int.MinValue
  }
}

trait Bounded[T] {
  val maxBound: T
  val minBound: T
}

object BoundedBits {
  implicit object IntIsBoundedBit extends BoundedBits[Int] {
    val bits = 32
  }
}

trait BoundedBits[T] {
  val bits : Int
}