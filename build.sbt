import AssemblyKeys._

import com.typesafe.sbt.SbtStartScript

seq(SbtStartScript.startScriptForClassesSettings: _*)

// In an SBT "full configuration" you would do something like:
// settings = SbtStartScript.startScriptForClassesSettings

name := "trilby"

version := "0.6.0"

scalaVersion := "2.10.1"

assemblySettings

libraryDependencies ++= Seq(
    "com.google.guava" % "guava" % "10.0",
    "jline" % "jline" % "2.9",
    "junit" % "junit" % "4.10",
    "log4j" % "log4j" % "1.2.17",
    "org.slf4j" % "slf4j-api" % "1.6.4",
    "org.slf4j" % "slf4j-log4j12" % "1.6.4",
    "org.slf4j" % "jcl-over-slf4j" % "1.6.4",
    "net.sf.trove4j" % "trove4j" % "3.0.3",
    "org.codehaus.jackson" % "jackson-core-asl" % "1.9.9",
    "org.eclipse.jetty" % "jetty-server" % "7.6.8.v20121106",
    "org.scalatest" % "scalatest_2.10" % "1.9.1" % "test"
)

