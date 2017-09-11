organization := "org.goldenport"

name := "arcadia"

version := "0.0.1-SNAPSHOT"

scalaVersion := "2.10.3"
// crossScalaVersions := Seq("2.10.39.2", "2.9.1")

scalacOptions += "-deprecation"

scalacOptions += "-unchecked"

scalacOptions += "-feature"

resolvers += "Scalaz Bintray Repo" at "http://dl.bintray.com/scalaz/releases"

resolvers += "Asami Maven Repository" at "http://www.asamioffice.com/maven"

libraryDependencies += "org.goldenport" %% "goldenport-record" % "1.1.25-SNAPSHOT"

libraryDependencies += "com.typesafe.play" %% "play-json" % "2.2.2" % "provided" exclude("org.scala-stm", "scala-stm_2.10.0")

libraryDependencies += "org.scalatra.scalate" %% "scalate-core" % "1.8.0"

libraryDependencies += "org.scalatest" %% "scalatest" % "2.0" % "test"

libraryDependencies += "junit" % "junit" % "4.10" % "test"

//
publishTo := Some(Resolver.file("asamioffice", file("target/maven-repository")))
