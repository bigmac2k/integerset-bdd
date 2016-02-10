package cc.sven.equivalences

/**
  * Created by scm on 04.02.16.
  */
sealed trait Either[A, B] {
  val isLeft : Boolean
  val isRight : Boolean
  def getLeft : Left[A, B]
  def getRight : Right[A, B]
}
final case class Left[A, B](a : A) extends Either[A, B] {
  val isLeft = true
  val isRight = false
  def getLeft = this
  def getRight = {assert(false, "tried to get right from left object"); null}
}
final case class Right[A, B](b : B) extends Either[A, B] {
  val isLeft = false
  val isRight = true
  def getLeft = {assert(false, "tried to get left from right object"); null}
  def getRight = this
}

object Either {
  object Implicits {
    implicit def eitherIsOrdered[A, B](implicit ordA: Ordering[A], ordB: Ordering[B]) = new Ordering[Either[A, B]] {
      def compare(a: Either[A, B], b: Either[A, B]) = (a, b) match {
        case (Left(l1), Left(l2)) => ordA.compare(l1, l2)
        case (Right(r1), Right(r2)) => ordB.compare(r1, r2)
        case (Left(_), Right(_)) => -1
        case (Right(_), Left(_)) => 1
      }
    }
  }
}
