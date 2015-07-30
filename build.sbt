name := "bdd"

version := "0.1"

scalaVersion := "2.11.7"

resolvers ++= Seq(
  "Sonatype Snapshots" at "http://oss.sonatype.org/content/repositories/snapshots",
  "Sonatype Releases" at "http://oss.sonatype.org/content/repositories/releases"
)

libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.12.4" % "test"

parallelExecution in Test := false
