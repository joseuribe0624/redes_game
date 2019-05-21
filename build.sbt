name := "redes_game"

version := "0.1"

scalaVersion := "2.12.8"

libraryDependencies += "org.scala-lang" % "scala-actors" % "2.11.8"

libraryDependencies += "org.scala-lang.modules" %% "scala-swing" % "2.1.1"

libraryDependencies ++= Seq(
  "com.typesafe.akka" %% "akka-actor" % "2.5.22",
  "com.typesafe.akka" %% "akka-testkit" % "2.5.22" % Test,
)
