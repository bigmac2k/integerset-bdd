package cc.sven.tlike

import cc.sven.bdd.CBDD

case class DepthPredicate(cutoff: Int) extends ((CBDD,Int,Int)=>Boolean) {
  override def toString(): String = s"Depth: $cutoff"
  def apply(node: CBDD, height: Int, depth: Int): Boolean = depth > cutoff
}

case class PrecisionPredicate(precision: Double) extends ((CBDD,Int,Int)=>Boolean) {
  override def toString(): String = s"Precision: $precision"

  def apply(node: CBDD, height: Int, depth: Int): Boolean = {
    def count(bdd: CBDD, height: Int): Long = {
      bdd.truePaths.map(l => 1L << (height - l.length)).sum
    }

    val approxSize = if (height == 64) Long.MaxValue else 1L << height
    val actualSize = count(node, height)

    (actualSize.toDouble / approxSize).abs >= precision
  }
}

