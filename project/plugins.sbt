logLevel := Level.Warn

addSbtPlugin("com.typesafe.sbt" % "sbt-native-packager" % "1.4.1")
addSbtPlugin("com.eed3si9n" % "sbt-assembly" % "0.14.10")
// addSbtPlugin( "net.virtual-void" % "sbt-dependency-graph" % "0.8.2" )

// maven publish
addSbtPlugin("com.jsuereth" % "sbt-pgp" % "2.0.0")
addSbtPlugin("org.xerial.sbt" % "sbt-sonatype" % "3.7")

// addSbtPlugin("com.github.gseitz" % "sbt-release" % "1.0.11")
addSbtPlugin("com.dwijnand" % "sbt-travisci" % "1.2.0")
