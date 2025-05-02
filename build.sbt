organization := "org.goldenport"

name := "arcadia"

version := "0.6.0"

scalaVersion := "2.12.7"
// crossScalaVersions := Seq("2.10.39.2", "2.9.1")

scalacOptions += "-deprecation"

scalacOptions += "-unchecked"

scalacOptions += "-feature"

// javacOptions ++= Seq("-source", "1.8", "-target", "1.8")

// resolvers += "Scalaz Bintray Repo" at "http://dl.bintray.com/scalaz/releases"

resolvers += "GitHab releases 2019" at "https://raw.github.com/asami/maven-repository/2019/releases"

resolvers += "GitHab releases 2020" at "https://raw.github.com/asami/maven-repository/2020/releases"

resolvers += "GitHab releases 2021" at "https://raw.github.com/asami/maven-repository/2021/releases"

resolvers += "GitHab releases 2022" at "https://raw.github.com/asami/maven-repository/2022/releases"

resolvers += "GitHab releases" at "https://raw.github.com/asami/maven-repository/2023/releases"

resolvers += "GitHub Packages" at "https://maven.pkg.github.com/asami/maven-repository"

resolvers += "Asami Maven Repository" at "http://www.asamioffice.com/maven"

// override goldenport-record
libraryDependencies += "org.goldenport" %% "goldenport-scala-lib" % "2.2.0"

libraryDependencies += "org.goldenport" %% "goldenport-record" % "2.2.0"

libraryDependencies += "com.typesafe.play" %% "play-json" % "2.6.10" % "provided" exclude("org.scala-stm", "scala-stm_2.10.0")

libraryDependencies += "org.scalatra.scalate" %% "scalate-core" % "1.9.6"

libraryDependencies += "org.scalatra.scalate" %% "scalamd" % "1.7.3"

libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.5" % "test"

libraryDependencies += "de.neuland-bfi" % "pug4j" % "2.3.1"

libraryDependencies += "junit" % "junit" % "4.10" % "test"

//
// AutoMkcol.globalSettings

publishTo := Some(
  "GitHub Packages" at "https://maven.pkg.github.com/asami/maven-repository"
)

credentials += Credentials(Path.userHome / ".sbt" / ".credentials")

publishMavenStyle := true
