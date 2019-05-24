organization := "org.goldenport"

name := "arcadia"

version := "0.1.2"

scalaVersion := "2.12.7"
// crossScalaVersions := Seq("2.10.39.2", "2.9.1")

scalacOptions += "-deprecation"

scalacOptions += "-unchecked"

scalacOptions += "-feature"

resolvers += "Scalaz Bintray Repo" at "http://dl.bintray.com/scalaz/releases"

resolvers += "Asami Maven Repository" at "http://www.asamioffice.com/maven"

// override goldenport-record
libraryDependencies += "org.goldenport" %% "goldenport-scala-lib" % "2.1.2"

libraryDependencies += "org.goldenport" %% "goldenport-record" % "2.1.2"

libraryDependencies += "com.typesafe.play" %% "play-json" % "2.6.10" % "provided" exclude("org.scala-stm", "scala-stm_2.10.0")

libraryDependencies += "org.scalatra.scalate" %% "scalate-core" % "1.8.0"

libraryDependencies += "org.scalatra.scalate" %% "scalamd" % "1.7.1"

libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.5" % "test"

libraryDependencies += "junit" % "junit" % "4.10" % "test"

//
AutoMkcol.globalSettings

credentials += Credentials(Path.userHome / ".sbt" / ".credentials")

publishTo <<= version { v: String =>
  val backlog = "https://everforth.backlog.jp/dav/APC/maven/"
  if (v.trim.endsWith("SNAPSHOT"))
    Some("Backlog snapshots" at backlog + "snapshots")
  else
    Some("Backlog releases" at backlog + "releases")
}
