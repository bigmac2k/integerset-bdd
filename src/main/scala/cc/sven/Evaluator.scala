package cc.sven
import java.io.{BufferedWriter, File, FileWriter}
import java.text.SimpleDateFormat
import java.util.Calendar

import cc.sven.bounded.{Bounded, BoundedBits, DynBoundedBits}
import cc.sven.tlike.{Castable, IntLikeSet}



object Evaluator {
  val file = new File(s"evaluation_${startTime}.csv")
  val writer = new BufferedWriter(new FileWriter(file))

  {
    sys.addShutdownHook(writer.close())
    writer.write("size_a;a_min;a_max;size_b;b_min;b_max;is_singleton;ref_size;ref_min;ref_max;sing_size;sing_min;" +
      "sing_max;height;height_bounds;prec;prec_bounds\n")
    writer.flush()
  }



  def evaluate[I,T](a: IntLikeSet[I,T], b: IntLikeSet[I,T], elems : Int, depths : Int)(implicit int : Integral[I], bounded : Bounded[I],
                                                            boundedBits : BoundedBits[I], dboundedBits : DynBoundedBits[T],
                                                            castTI : Castable[T, Pair[Int, I]], castIT : Castable[Pair[Int, I], T]): IntLikeSet[I,T] = {

   /* val precise = for{
      a <- a.set
      b <- b.set
    } yield castIT(boundedBits.bits, int.times(a, b)) */
    val precise = Set()

    val reference = a.mul(elems, depths)(b)

    /*
    val heightResultBounds = testGeneralHeight(a, b, true)
    val precisionResultBounds = testGeneralPrecision(a, b, true)
    val heightResult = testGeneralHeight(a, b, false)
    val precisionResult= testGeneralPrecision(a, b, false) */
    val aSize = a.sizeBigInt
    val bSize = b.sizeBigInt

    val isSingleton = aSize == 1 || bSize == 1
    val (singleton, singletonSize, singletonMin, singletonMax) = if (aSize == 1) {
      val elem = a.randomElement()
      testSingleton(b, elem)
    } else if (bSize == 1) {
      val elem = b.randomElement()
      testSingleton(a, elem)
    } else {
      (null, -1, -1, -1)
    }

    val (heightResultBounds, precisionResultBounds, heightResult, precisionResult) = if (aSize > 1 && bSize > 1) {
      (testGeneralHeight(a, b, true), testGeneralPrecision(a, b, true), testGeneralHeight(a, b, false), testGeneralPrecision(a, b, false))
    } else {
      (List(), List(), List(), List())
    }

    writer.write(s"$aSize;${a.min};${a.max};$bSize;${b.min};${b.max};${isSingleton};${reference.sizeBigInt};${reference.min};" +
      s"${reference.max};$singletonSize;$singletonMin;$singletonMax;${formatPrecisionParameters(heightResult)};" +
      s"${formatPrecisionParameters(heightResultBounds)};${formatPrecisionParameters(precisionResult)};" +
      s"${formatPrecisionParameters(precisionResultBounds)}\n")
    writer.flush()

    singleton
  }

  def formatPrecisionParameters[T,U](list: List[(T,U)]): String = {
    list.map(x=>s"${x._1}=>${x._2}").mkString(",")
  }

  def testGeneralHeight[I,T](a: IntLikeSet[I,T], b: IntLikeSet[I,T], findBounds: Boolean)(implicit int : Integral[I], bounded : Bounded[I],
                                               boundedBits : BoundedBits[I], dboundedBits : DynBoundedBits[T],
                                               castTI : Castable[T, Pair[Int, I]], castIT : Castable[Pair[Int, I], T]): List[(Int,(BigInt, T, T))] = {
    (0 to 10 by 5).map({ p =>
      println(s"$currentTime: Height: $p, findBound: $findBounds")
      val res = a.mulPredicate(IntLikeSet.heightPredicate(p))(findBounds)(b)
      (p, (res.sizeBigInt, res.min, res.max))
    }).toList

  }

  def testGeneralPrecision[I,T](a: IntLikeSet[I,T], b: IntLikeSet[I,T], findBounds: Boolean)(implicit int : Integral[I], bounded : Bounded[I],
                                                  boundedBits : BoundedBits[I], dboundedBits : DynBoundedBits[T],
                                                  castTI : Castable[T, Pair[Int, I]], castIT : Castable[Pair[Int, I], T]): List[(Double,(BigInt, T, T))] = {

    (0.125 to 1 by 0.25).map({ p =>
      println(s"$currentTime: Precision: $p, findBound: $findBounds")
      val res = a.mulPredicate(IntLikeSet.precisionPredicate(p))(findBounds)(b)
      (p, (res.sizeBigInt, res.min, res.max))
    }).toList

  }

  def testSingleton[I,T](a: IntLikeSet[I,T], b: T)(implicit int : Integral[I], bounded : Bounded[I],
                                           boundedBits : BoundedBits[I], dboundedBits : DynBoundedBits[T],
                                           castTI : Castable[T, Pair[Int, I]], castIT : Castable[Pair[Int, I], T]): (IntLikeSet[I,T], BigInt, T, T) = {
    println(s"$currentTime: Singleton multiplication started")
    val res = a.mulSingleton4(b)
    println(s"$currentTime: Singleton multiplication finished")
    (res, res.sizeBigInt, res.min, res.max)

  }

  def currentTime = {
    val now = Calendar.getInstance().getTime
    val format = new SimpleDateFormat("hh:mm:ss:SS")
    format.format(now)
  }

  def startTime = {
    val now = Calendar.getInstance().getTime
    val format = new SimpleDateFormat("dd-MM_hh-mm-ss")
    format.format(now)
  }
}