name := "evalEdn"

version := "1.0"

scalaVersion := "2.11.7"

libraryDependencies ++= Seq(
  "net.cabworks" %% "edn4scala" % "0.1",
  "org.scalatest" %% "scalatest" % "2.2.4" % "test",
  "org.scalacheck" %% "scalacheck" % "1.12.5" % "test"
)