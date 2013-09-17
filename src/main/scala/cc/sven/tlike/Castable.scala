package cc.sven.tlike

trait Castable[A, B] {
  def apply(a : A) : B
}

object Castable {
  implicit def tToT[T] = new Castable[T, T] {
    def apply(t :T) : T = t
  }
  /*implicit class As[A, B](a : A)(implicit castb : Castable[A, B]) {
    def as[C](implicit eq : C =:= B) = castb.cast(a)
  }*/
}