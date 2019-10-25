Common.settings
Common.fxSettings

import Common._

name := "memnets-fx"

description := "MemNets Community Edition"

mainClass in (Compile, run) := Some("memnets.fx.demo.LorentzFX")

// override default versioning
version := {
  FxVersion match {
    case `fx8` =>
      MemNetsVersion
    case default =>
      s"$MemNetsVersion-$default"
  }
}

// ScriptBuilderTest needs this
fork in (Test) := true

libraryDependencies ++= {
  FxVersion match {
    case `fx8` =>
      // controlsfx 8.40.15 compiled against Java 9 (modules)
      Seq(
        "org.controlsfx" % "controlsfx" % "8.40.15",
        "org.scalafx" %% "scalafx" % "8.0.192-R14",
        "org.fxyz3d" % "fxyz3d" % "0.4.0")
    case `fx11` =>
      // fxyz 0.5.2 totally breaks fx11.  has own dep on fx12...
      Seq(
        "org.controlsfx" % "controlsfx" % "11.0.0",
        "org.scalafx" %% "scalafx" % "11-R16",
        "org.fxyz3d" % "fxyz3d" % "0.5.1")
    case `fx12` =>
      Seq(
        "org.controlsfx" % "controlsfx" % "11.0.0",
        "org.scalafx" %% "scalafx" % "12.0.2-R18",
        "org.fxyz3d" % "fxyz3d" % "0.5.2")
    case default =>
      ???
  }
}

// code editor
libraryDependencies ++= {
  Seq(
    "org.fxmisc.richtext" % "richtextfx" % "0.10.2",
    "org.scala-lang.modules" %% "scala-parser-combinators" % "1.1.2",
    "org.scala-lang" % "scala-reflect" % scalaVersion.value,
    "org.scala-lang" % "scala-compiler" % scalaVersion.value
  )
}

resolvers ++= Seq(
  "Jzy3d Snapshots".at("http://maven.jzy3d.org/releases") // used by fxyz for jzy3d
)
