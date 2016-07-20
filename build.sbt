name := "bdd"

version := "0.1"

scalaVersion := "2.11.7"

resolvers ++= Seq(
  "Sonatype Snapshots" at "http://oss.sonatype.org/content/repositories/snapshots",
  "Sonatype Releases" at "http://oss.sonatype.org/content/repositories/releases"
)

//libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.12.4" % "test"

libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.13.0"

parallelExecution in Test := false
test in assembly := {}



initialCommands in console := "import cc.sven.bdd._; var a = Node(Node(Node(True, False),Node(True, False)),False); var b = Node(Node(Node(True, False),Node(True, False)),Node(Node(True, False),False)); var c = Node(Node(Node(True, False),Node(False, True)),Node(Node(True, False),False))"

