organization := "org.goldenport"

name := "arcadia"

version := "0.3.2"

scalaVersion := "2.10.3"
// crossScalaVersions := Seq("2.10.39.2", "2.9.1")

scalacOptions += "-deprecation"

scalacOptions += "-unchecked"

scalacOptions += "-feature"

// resolvers += "Scalaz Bintray Repo" at "http://dl.bintray.com/scalaz/releases"

resolvers += "GitHab releases 2019" at "https://raw.github.com/asami/maven-repository/2019/releases"

resolvers += "GitHab releases 2020" at "https://raw.github.com/asami/maven-repository/2020/releases"

resolvers += "GitHab releases 2022" at "https://raw.github.com/asami/maven-repository/2022/releases"

resolvers += "GitHab releases" at "https://raw.github.com/asami/maven-repository/2023/releases"

resolvers += "GitHub Packages" at "https://maven.pkg.github.com/asami/maven-repository"

resolvers += "Asami Maven Repository" at "http://www.asamioffice.com/maven"

// override goldenport-record
libraryDependencies += "org.goldenport" %% "goldenport-scala-lib" % "1.4.3"

libraryDependencies += "org.goldenport" %% "goldenport-record" % "1.4.0"

libraryDependencies += "com.typesafe.play" %% "play-json" % "2.2.2" % "provided" exclude("org.scala-stm", "scala-stm_2.10.0")

libraryDependencies += "org.scalatra.scalate" %% "scalate-core" % "1.8.0"

libraryDependencies += "org.fusesource.scalamd" %% "scalamd" % "1.6"

libraryDependencies += "de.neuland-bfi" % "pug4j" % "2.3.1"

libraryDependencies += "org.scalatest" %% "scalatest" % "2.2.6" % "test"

libraryDependencies += "junit" % "junit" % "4.10" % "test"

//
// AutoMkcol.globalSettings

publishTo := Some(
  "GitHub Packages" at "https://maven.pkg.github.com/asami/maven-repository"
)

credentials += Credentials(Path.userHome / ".sbt" / ".credentials")

publishMavenStyle := true
