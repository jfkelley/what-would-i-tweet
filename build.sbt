name := "what-would-i-tweet"

version := "1.0-SNAPSHOT"

libraryDependencies ++= Seq(
  jdbc,
  anorm,
  cache
)     

libraryDependencies += "org.twitter4j" % "twitter4j-core" % "3.0.5"

libraryDependencies += "org.postgresql" % "postgresql" % "9.3-1100-jdbc4"

play.Project.playScalaSettings
