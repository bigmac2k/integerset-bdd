package cc.sven.bounded

object DynBounded {
  implicit def boundedToDynBounded[T](implicit bounded : Bounded[T]) : DynBounded[T] = new DynBounded[T] {
    def dMaxBound(t : T) : T = bounded.maxBound
    def dMinBound(t : T) : T = bounded.minBound
  }
}

trait DynBounded[T] {
  def dMaxBound(t : T) : T
  def dMinBound(t : T) : T
}