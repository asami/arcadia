organization := "org.goldenport"

name := "arcadia"

version := "0.2.6"

scalaVersion := "2.10.3"
// crossScalaVersions := Seq("2.10.39.2", "2.9.1")

scalacOptions += "-deprecation"

scalacOptions += "-unchecked"

scalacOptions += "-feature"

// resolvers += "Scalaz Bintray Repo" at "http://dl.bintray.com/scalaz/releases"

resolvers += "GitHab releases 2019" at "https://raw.github.com/asami/maven-repository/2019/releases"

resolvers += "GitHab releases 2020" at "https://raw.github.com/asami/maven-repository/2020/releases"

resolvers += "GitHab releases" at "https://raw.github.com/asami/maven-repository/2022/releases"

resolvers += "Asami Maven Repository" at "http://www.asamioffice.com/maven"

// override goldenport-record
libraryDependencies += "org.goldenport" %% "goldenport-scala-lib" % "1.3.38"

libraryDependencies += "org.goldenport" %% "goldenport-record" % "1.3.61"

libraryDependencies += "com.typesafe.play" %% "play-json" % "2.2.2" % "provided" exclude("org.scala-stm", "scala-stm_2.10.0")

libraryDependencies += "org.scalatra.scalate" %% "scalate-core" % "1.8.0"

libraryDependencies += "org.fusesource.scalamd" %% "scalamd" % "1.6"

libraryDependencies += "org.scalatest" %% "scalatest" % "2.2.6" % "test"

libraryDependencies += "junit" % "junit" % "4.10" % "test"

//
// AutoMkcol.globalSettings

// credentials += Credentials(Path.userHome / ".sbt" / ".credentials")

val mavenrepo = settingKey[String]("mavenrepo")

mavenrepo := sys.env.getOrElse("PUBLISH_MAVEN_REPO", default = "target/maven-repository")

publishTo <<= mavenrepo { v: String =>
  Some(Resolver.file("file", file(v)))
}
