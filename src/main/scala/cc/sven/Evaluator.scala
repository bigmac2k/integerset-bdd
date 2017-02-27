package cc.sven
import java.io.{BufferedWriter, File, FileWriter}
import java.text.SimpleDateFormat
import java.util.Calendar

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

   /* val precise = for{
      a <- a.set
      b <- b.set
    } yield castIT(boundedBits.bits, int.times(a, b)) */
    val precise = Set()

    val reference = a.mul(elems, depths)(b)

    val heightResultBounds = testGeneralHeight(a, b, true)
    val precisionResultBounds = testGeneralPrecision(a, b, true)
    val heightResult = testGeneralHeight(a, b, false)
    val precisionResult= testGeneralPrecision(a, b, false)
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

    writer.write(s"$aSize;$bSize;${isSingleton};${precise.size};${reference.sizeBigInt};${formatPrecisionParameters(heightResult)};" +
      s"${formatPrecisionParameters(heightResultBounds)};${formatPrecisionParameters(precisionResult)};" +
      s"${formatPrecisionParameters(precisionResultBounds)};${singletonResult}\n")
    writer.flush()
  }

  def formatPrecisionParameters[T](list: List[(T,BigInt)]): String = {
    list.map(x=>s"${x._1}=>${x._2}").mkString(",")
  }

  def testGeneralHeight[I,T](a: IntLikeSet[I,T], b: IntLikeSet[I,T], findBounds: Boolean)(implicit int : Integral[I], bounded : Bounded[I],
                                               boundedBits : BoundedBits[I], dboundedBits : DynBoundedBits[T],
                                               castTI : Castable[T, Pair[Int, I]], castIT : Castable[Pair[Int, I], T]): List[(Int,BigInt)] = {
    (0 to 20 by 5).map({p=>println(s"$currentTime: Height: $p, findBound: $findBounds");(p, a.mulPredicate(IntLikeSet.heightPredicate(p))(findBounds)(b).sizeBigInt)}).toList

  }

  def testGeneralPrecision[I,T](a: IntLikeSet[I,T], b: IntLikeSet[I,T], findBounds: Boolean)(implicit int : Integral[I], bounded : Bounded[I],
                                                  boundedBits : BoundedBits[I], dboundedBits : DynBoundedBits[T],
                                                  castTI : Castable[T, Pair[Int, I]], castIT : Castable[Pair[Int, I], T]): List[(Double,BigInt)] = {

    (0.125 to 1 by 0.125).map({p=>println(s"$currentTime: Precision: $p, findBound: $findBounds");(p, a.mulPredicate(IntLikeSet.precisionPredicate(p))(findBounds)(b).sizeBigInt)}).toList

  }

  def testSingleton[I,T](a: IntLikeSet[I,T], b: T)(implicit int : Integral[I], bounded : Bounded[I],
                                           boundedBits : BoundedBits[I], dboundedBits : DynBoundedBits[T],
                                           castTI : Castable[T, Pair[Int, I]], castIT : Castable[Pair[Int, I], T]): BigInt = {
    println(s"$currentTime: Singleton multiplication started")
    val res = a.mulSingleton4(b).sizeBigInt
    println(s"$currentTime: Singleton multiplication finished")
    res

  }

  def currentTime = {
    val now = Calendar.getInstance().getTime
    val format = new SimpleDateFormat("hh:mm:ss:SS")
    format.format(now)
  }
}