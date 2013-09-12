package cc.sven.bounded

object Bounded {
  implicit val intIsBounded = new JIntIsBounded
  implicit val integerIsBounded = new JIntegerIsBounded
}

trait Bounded[T] {
  val maxBound: T
  val minBound: T
}

object BoundedBits {
  implicit val intIsBoundedBits = new JIntIsBoundedBits
  implicit val integerIsBoundedBits = new JIntegerIsBoundedBits
}

trait BoundedBits[T] {
  val bits: Int
}