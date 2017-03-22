name := "bdd"

version := "0.1"

//scalaVersion := "2.11.7"
scalaVersion := "2.12.1"

resolvers ++= Seq(
  "Sonatype Snapshots" at "http://oss.sonatype.org/content/repositories/snapshots",
  "Sonatype Releases" at "http://oss.sonatype.org/content/repositories/releases"
)

//libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.12.4" % "test"

libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.13.4"

libraryDependencies += "com.github.nscala-time" %% "nscala-time" % "2.16.0"



parallelExecution in Test := false
test in assembly := {}

initialCommands in console := """
  |import cc.sven.bdd._
  |import cc.sven.tlike._
  |import cc.sven.intset._""".stripMargin
