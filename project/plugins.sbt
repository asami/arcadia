resolvers += "Sonatype OSS Snapshots" at "https://oss.sonatype.org/content/groups/scala-tools/"

addSbtPlugin("com.typesafe.sbteclipse" % "sbteclipse-plugin" % "2.4.0")

addSbtPlugin("org.ensime" % "ensime-sbt-cmd" % "0.1.2")

// http://tototoshi.hatenablog.com/entry/2015/03/12/205444
addSbtPlugin("com.github.tototoshi" % "sbt-automkcol" % "1.5.0")

// addSbtPlugin("net.databinder" %% "giter8-plugin" % "0.3.2")

// addSbtPlugin("no.arktekk.sbt" % "aether-deploy" % "0.6")

addSbtPlugin("com.eed3si9n" % "sbt-buildinfo" % "0.5.0")
