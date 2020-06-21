name := "kmeans-module"

version := "0.1"

scalaVersion := "2.12.6"

libraryDependencies ++= Seq(
  "com.github.haifengl" % "smile-core" % "1.5.1",
  "com.github.haifengl" %% "smile-scala" % "1.5.1"
)

libraryDependencies += "ch.qos.logback" % "logback-classic" % "1.1.3" % Runtime //removes warning