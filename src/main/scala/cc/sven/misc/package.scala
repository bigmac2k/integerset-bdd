package cc.sven

import cc.sven.tlike.IntLikeSet
import java.nio.ByteBuffer
import scala.math.BigInt

package object misc {
  import scala.language.higherKinds
  import scala.language.implicitConversions

  def unsignedLongToBigInt(l : Long) : BigInt = {
    BigInt(ByteBuffer.allocate(9).putLong(1, l).array)
  }

  trait Monad[M[_]] {
    def unit[A](a : A) : M[A]
    def bind[A, B](m : M[A], k : A => M[B]) : M[B]
  }
  object Monad {
    def apply[A[_] : Monad] = implicitly[Monad[A]]
  }

  implicit class Monadic[A, M[_]](x : M[A])(implicit monad : Monad[M]) {
    def flatMap[B](k : A => M[B]) : M[B] = monad.bind(x, k)
    def map[B](k : A => B) : M[B] = monad.bind(x, (m : A) => monad.unit(k(m)))
  }

  implicit object ListIsMonad extends Monad[List] {
    def unit[A](a : A) : List[A] = List(a)
    def bind[A, B](ms_ : List[A], k : A => List[B]) : List[B] = ms_ match {
      case Nil => List[B]()
      case m :: ms => k(m) ++ bind(ms, k)
    }
  }

  def sequence[M[_] : Monad, A](ms : List[M[A]])(implicit monad : Monad[M]) : M[List[A]] = (ms :\ monad.unit(List[A]())){
    (m, m_) => for(x <- new Monadic(m)(monad); ms <- new Monadic(m_)(monad)) yield (x :: ms)
  }

  /*def sizeMetric[I, T](bdd : IntLikeSet[I, T], sections : Int) = {
    val paths = sequence(List.fill(sections)(List(true, false))).map(bdd.set.cbdd.partialEval(_))
    paths map (x => x map )
  }*/
}
