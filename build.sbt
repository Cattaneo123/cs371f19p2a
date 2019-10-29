name := "iterators-scala"

version := "0.3"

scalaVersion := "2.12.9"

scalacOptions ++= Seq("-deprecation", "-feature", "-unchecked")

libraryDependencies ++= Seq(
  "org.scalatest"     %% "scalatest"  % "3.0.5"  % Test,
  "com.storm-enroute" %% "scalameter" % "0.10.1" % Test,
)

libraryDependencies += "org.apache.commons" % "commons-collections4" % "4.4"

testFrameworks += new TestFramework("org.scalameter.ScalaMeterFramework")

logBuffered := false

parallelExecution in Test := false

enablePlugins(JavaAppPackaging)
