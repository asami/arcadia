organization := "org.goldenport"

name := "arcadia"

version := "0.0.10"

scalaVersion := "2.10.3"
// crossScalaVersions := Seq("2.10.39.2", "2.9.1")

scalacOptions += "-deprecation"

scalacOptions += "-unchecked"

scalacOptions += "-feature"

resolvers += "Scalaz Bintray Repo" at "http://dl.bintray.com/scalaz/releases"

resolvers += "Asami Maven Repository" at "http://www.asamioffice.com/maven"

// override goldenport-record
libraryDependencies += "org.goldenport" %% "goldenport-scala-lib" % "1.2.5"

libraryDependencies += "org.goldenport" %% "goldenport-record" % "1.2.5"

libraryDependencies += "com.typesafe.play" %% "play-json" % "2.2.2" % "provided" exclude("org.scala-stm", "scala-stm_2.10.0")

libraryDependencies += "org.scalatra.scalate" %% "scalate-core" % "1.8.0"

libraryDependencies += "org.fusesource.scalamd" %% "scalamd" % "1.6"

libraryDependencies += "org.scalatest" %% "scalatest" % "2.0" % "test"

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
