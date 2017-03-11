package cc.sven.interval


sealed trait StridedInterval[+T]

case object EmptyStridedIval extends StridedInterval[Nothing]

case class FilledStridedIval[+T](stride: T, lo : T, hi : T) extends StridedInterval[T] {
  override def toString = s"$stride[" + lo + " .. " + hi + "]"
  override def equals(that : Any) = that match {
    case ival : FilledStridedIval[T] => lo == ival.lo && hi == ival.hi && stride == ival.stride
    case _ => false
  }
  override def hashCode = (stride, lo, hi).hashCode
}
