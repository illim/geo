name := "geo"

version := "1.0"

scalaVersion := "2.10.1"

resolvers ++= Seq(
 "Sonatype Repository" at "http://oss.sonatype.org/content/repositories/releases")


libraryDependencies ++= Seq(
 "org.lwjgl.lwjgl" % "lwjgl" % "2.8.2",
 "org.lwjgl.lwjgl" % "lwjgl_util" % "2.8.2",
 "org.lwjgl.lwjgl" % "lwjgl-platform" % "2.8.2" classifier "natives-windows",
 "org.scalatest" % "scalatest_2.10" % "1.9.1" % "test")

javaOptions in run ++= Seq("-Djava.library.path=C:/Users/mdoboi/.ivy2/cache/org.lwjgl.lwjgl/lwjgl-platform/jars/", "-Dorg.lwjgl.util.Debug=true")

fork in run := true

