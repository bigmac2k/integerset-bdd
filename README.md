# BDDStab - BDD-based Integer Set Library

This library contains a BDD-based integer set implementation that efficiently handles large sets of integers of the fixed or dynamic bit-width, i.e., 2^n-bit integers as well as n-bit integers.
API documentation is available at [github](https://bigmac2k.github.io/integerset-bdd/latest/api/cc/sven/index.html).

More information, as well as publication are available at the website of the [Hamburg University of Technology's Institute for Softwaresystems](https://www.tuhh.de/sts/research/projects/bddstab.html).

## Intended Use

Primarily, this library is intended to be used in abstract-interpretation-based value analysis tools.
It contains transfer functions on the BDD-based integer sets that operate on the underlying BDDs-only, i.e., do not operate on the represented integers.
An example use of the library is available in the [Jakstab BDDStab Adapter](https://github.com/bigmac2k/jakstab).

## Example

To use the library, create an empty directory. Create the following files:

-----------------------------------------------------------------------

project/assembly.sbt:
```scala
addSbtPlugin("com.eed3si9n" % "sbt-assembly" % "0.14.5")
```

-----------------------------------------------------------------------

-----------------------------------------------------------------------

build.sbt:
```scala
name := "bddusage"
scalaVersion := "2.12.2"
lazy val root = Project("root", file(".")).dependsOn(bddproject)
lazy val bddproject = RootProject(uri("https://github.com/bigmac2k/integerset-bdd.git"))
```

-----------------------------------------------------------------------

-----------------------------------------------------------------------

src/main/scala/usage.scala:
```scala
import cc.sven.tlike._
import cc.sven.constraint._
import scala.collection.immutable.HashMap
object Main extends App {
//Empty 64-bit integer set
val empty = IntLikeSet.apply[Long, Long](64)
//All 64-bit integers that are not in empty = all 64-bit integers
val top = !empty
//Singleton set containing 0
val zero = IntLikeSet.range[Long, Long](64, 0, 0)
//Singleton set containing 1
val one = IntLikeSet.range[Long, Long](64, 1, 1)
//Bitwise boolean negation of all bits in one
val negtwo = one.bNot
//Bitweise AND of all 64-bit integers with negtwo
//Should give all even integers since LSB is bit masked out
val evens = top.bAnd(negtwo)
//Add one to all even integers without wrap-around, store in odds
val (odds, _) = evens.plusWithCarry(one)
//All positive even integers
val evensPos = {
  //Mapping between variables, named using integers, and their contents
  val m = HashMap(0 -> evens, 1 -> zero)
  //Greater or equal (GTE) of two variables
  val t = GTE(0, 1).solveIntLike[Long, Long](m)
  //Select the constrained value for variable 0
  t(0)
}
//All positive odd integers
val oddsPos = {
  val m = HashMap(0 -> odds, 1 -> zero)
  val t = GTE(0, 1).solveIntLike[Long, Long](m)
  t(0)
}
//Add all odd integers to all even integers, store in odds_
val (odds_, _) = evens.plusWithCarry(odds)
//Output results
println(s"all positive evens:\n${evensPos}\n")
println(s"all positive odds:\n${oddsPos}\n")
println(s"intersection:\n${evensPos intersect oddsPos}\n")
println(s"union:\n${evensPos union oddsPos}\n")
println(s"evens + odds == odds:\n${odds_ == odds}\n")
}
```

-----------------------------------------------------------------------

Then execute the following command using the scala build tool:

```bash
$ sbt assembly
$ time java -jar target/scala-2.12/bddusage-assembly-0.1-SNAPSHOT.jar
```

The output may be as follows:
```
... all positive evens: Set\[4611686018427387904\](0, MANYVAL,
9223372036854775806)

all positive odds: Set\[4611686018427387904\](1, MANYVAL,
9223372036854775807)

intersection: Set\[0\]()

union: Set\[9223372036854775808\](0, MANYVAL, 9223372036854775807)

evens + odds == odds: true

java -jar 1,01s user 0,04s system 165
```
