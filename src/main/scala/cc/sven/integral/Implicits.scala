package cc.sven.integral

object Implicits {
  implicit val integerIsIntegral = new JIntegerIsIntegral
  implicit val integerIsOrdering = new JIntegerOrdering
}