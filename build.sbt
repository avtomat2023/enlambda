name := "enlambda"
version := "0.1"

scalaSource in Compile := baseDirectory.value / "main"
scalaSource in Test := baseDirectory.value / "test"

scalaVersion := "2.11.8"
scalacOptions ++= Seq("-unchecked", "-deprecation", "-feature")

libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % "2.2.6" % "test",
  "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.4"
)

initialCommands in console := "import enlambda._"
