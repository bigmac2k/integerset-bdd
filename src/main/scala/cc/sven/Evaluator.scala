package cc.sven
import java.io.{BufferedWriter, File, FileWriter}

import cc.sven.bounded.{Bounded, BoundedBits, DynBoundedBits}
import cc.sven.tlike.{Castable, IntLikeSet}



object Evaluator {
  {
    sys.addShutdownHook(writer.close())
  }

  val file = new File(s"evaluation_${System.currentTimeMillis()}.csv")
  val writer = new BufferedWriter(new FileWriter(file))

  def evaluate[I,T](a: IntLikeSet[I,T], b: IntLikeSet[I,T], elems : Int, depths : Int)(implicit int : Integral[I], bounded : Bounded[I],
                                                            boundedBits : BoundedBits[I], dboundedBits : DynBoundedBits[T],
                                                            castTI : Castable[T, Pair[Int, I]], castIT : Castable[Pair[Int, I], T]) = {

    val precise = for{
      a <- a.set
      b <- b.set
    } yield castIT(boundedBits.bits, int.times(a, b))


    val reference = a.mul(elems, depths)(b)

    val heightResult = testGeneralHeight(a, b)
    val precisionResult = testGeneralPrecision(a, b)
    val aSize = a.sizeBigInt
    val bSize = b.sizeBigInt

    val isSingleton = aSize == 1 || bSize == 1
    val singletonResult = if (aSize == 1) {
      val elem = a.randomElement()
      testSingleton(b, elem)
    } else if (bSize == 1) {
      val elem = b.randomElement()
      testSingleton(a, elem)
    } else {
      -1
    }

    writer.write(s"$aSize;$bSize;${isSingleton};${precise.size};${reference.size};${formatPrecisionParameters(heightResult)};" +
      s"${formatPrecisionParameters(precisionResult)};${singletonResult}\n")
    writer.flush()
  }

  def formatPrecisionParameters[T](list: List[(T,Int)]): String = {
    list.map(x=>s"${x._1}=>${x._2}").mkString(",")
  }

  def testGeneralHeight[I,T](a: IntLikeSet[I,T], b: IntLikeSet[I,T])(implicit int : Integral[I], bounded : Bounded[I],
                                               boundedBits : BoundedBits[I], dboundedBits : DynBoundedBits[T],
                                               castTI : Castable[T, Pair[Int, I]], castIT : Castable[Pair[Int, I], T]): List[(Int,Int)] = {
    (0 to 30 by 5).map(p=>(p, a.mulPredicate(IntLikeSet.heightPredicate(p))(true)(b).size)).toList
  }

  def testGeneralPrecision[I,T](a: IntLikeSet[I,T], b: IntLikeSet[I,T])(implicit int : Integral[I], bounded : Bounded[I],
                                                  boundedBits : BoundedBits[I], dboundedBits : DynBoundedBits[T],
                                                  castTI : Castable[T, Pair[Int, I]], castIT : Castable[Pair[Int, I], T]): List[(Double,Int)] = {

    (0.125 to 1 by 0.125).map(p=>(p, a.mulPredicate(IntLikeSet.precisionPredicate(p))(true)(b).size)).toList
  }

  def testSingleton[I,T](a: IntLikeSet[I,T], b: T)(implicit int : Integral[I], bounded : Bounded[I],
                                           boundedBits : BoundedBits[I], dboundedBits : DynBoundedBits[T],
                                           castTI : Castable[T, Pair[Int, I]], castIT : Castable[Pair[Int, I], T]): BigInt = {
    a.mulSingleton4(b).size
  }
}