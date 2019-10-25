import sbt.Keys._
import sbt._

object Common {
  import scala.xml.transform._
  import scala.xml.{Node => XmlNode, NodeSeq => XmlNodeSeq, _}

  val MemNetsVersion = "1.0.0"

  val fx8 = "8.0"
  val fx11 = "11.0.2"
  val fx12 = "12.0.2"
  // NOTE: must set FxVersion to one of the above variables (not just string values!)

  // fx8 is the easier default.  no java runtime flags (--module) needed.
  // val FxVersion = fx8
  val FxVersion = fx12

  // if not using the default, please see "javafx11plus runtime args.txt"

  // NOTE: changes to env vars will not be reflected at command prompt until close and re-open.
  val fxLibPath = sys.env.get("JAVAFX_LIB") match {
    case Some(path) =>
      println("Environmental variable JAVAFX_LIB = " + path)
      println("Environmental variable JAVA_HOME = " + sys.env.getOrElse("JAVA_HOME", "not set"))
      println("Java version = " + sys.props("java.version"))
      file(path)
    case None =>
      val fxmsg = "Environmental variable JAVAFX_LIB is not set"
      println(fxmsg)
      println("Environmental variable JAVA_HOME = " + sys.env.getOrElse("JAVA_HOME", "not set"))
      println("Java version = " + sys.props("java.version"))
      file(fxmsg)
  }
  lazy val fxSettings = Seq(
    javaOptions ++= {
      FxVersion match {
        case `fx8` =>
          Seq()
        case default =>
          Seq(
            "--module-path",
            s"$fxLibPath",
            "--add-modules=ALL-MODULE-PATH",
            "--add-opens=javafx.graphics/com.sun.javafx.css=ALL-UNNAMED",
            "--add-exports=javafx.graphics/com.sun.javafx.scene=ALL-UNNAMED",
            "--add-exports=javafx.graphics/com.sun.javafx.scene.traversal=ALL-UNNAMED",
            "--add-exports=javafx.graphics/com.sun.javafx.css=ALL-UNNAMED",
            "--add-exports=javafx.graphics/com.sun.javafx.geom=ALL-UNNAMED"
          )
      }
    },
    libraryDependencies ++= {
      FxVersion match {
        case `fx8` =>
          Seq()
        case default =>
          val osName = sys.props("os.name") match {
            case n if n.startsWith("Linux")   => "linux"
            case n if n.startsWith("Mac")     => "mac"
            case n if n.startsWith("Windows") => "win"
            case _                            => throw new Exception("Unknown platform!")
          }

          // JavaFX11+ dependencies, mark as "provided", so they can be later removed from published POM
          Seq("base", "controls", "fxml", "graphics", "media", "swing", "web").map(m =>
            ("org.openjfx" % s"javafx-$m" % FxVersion % "provided").classifier(osName))
      }
    }
  )

  lazy val settings = Seq(
    organization := "com.memnets",
    version := MemNetsVersion,
    crossScalaVersions := {
      // 2.13.x only works with fx12
      // 2.13.1 has major bug: call to ScriptManager.getEngine("scala") blows up
      FxVersion match {
        case `fx12` =>
//          Seq("2.12.9", "2.13.1")
          Seq("2.12.9", "2.13.0")
        case default =>
          Seq("2.12.9")
      }
    },
    scalaVersion := crossScalaVersions.value.last,
    // gathers all dependent jars locally in /lib_managed
    retrieveManaged := true,
    autoAPIMappings := true,
    manifestSetting,
    fork in (run) := true,
    fork in (Test) := false,
    parallelExecution in Test := false,
    // print junit-style XML for CI
    testOptions in Test += {
      val t = (target in Test).value
      Tests.Argument(TestFrameworks.ScalaTest, "-u", s"$t/junitxmldir")
    },
    shellPrompt in ThisBuild := { state =>
      "sbt:" + Project.extract(state).currentRef.project + "> "
    },
    scalacOptions ++= Seq(
      "-target:jvm-1.8",
      "-unchecked",
      "-feature",
      "-language:_",
      "-deprecation",
      "-encoding",
      "utf8"
    ),
    javacOptions ++= Seq(
      "-source",
      "1.8",
      "-target",
      "1.8",
      "-Xlint"
    ),
    // JAVA, NOTE: uses only one '%' in first position
    libraryDependencies ++= Seq(
      // want default logging w/o forcing users to know details.  exclude dependency if desire alternative
      "ch.qos.logback" % "logback-classic" % "1.2.3",
      "junit" % "junit" % "4.8.2" % "test"
    ),
    // SCALA
    libraryDependencies ++= Seq(
      "org.scalanlp" %% "breeze" % "1.0",
      // native support is optional.  see https://github.com/scalanlp/breeze build.sbt for comments
      "org.scalanlp" %% "breeze-natives" % "1.0" % "provided",
      "com.typesafe.scala-logging" %% "scala-logging" % "3.9.2",
      "org.scalactic" %% "scalactic" % "3.0.8" % "test",
      "org.scalatest" %% "scalatest" % "3.0.8" % "test"
    ),
    // FIX for https://www.slf4j.org/codes.html#multiple_bindings
    excludeDependencies += "org.slf4j" % "slf4j-simple",
    /* different version to clean POM, but going with one below
      // Use `pomPostProcess` to remove dependencies marked as "provided" from publishing in POM
      // This is to avoid dependency on wrong OS version JavaFX libraries [Issue #289]
      // See also [https://stackoverflow.com/questions/27835740/sbt-exclude-certain-dependency-only-during-publish]
      pomPostProcess := { node: XmlNode =>
        new RuleTransformer(new RewriteRule {
          override def transform(node: XmlNode): XmlNodeSeq = node match {
            case e: Elem if e.label == "dependency" && e.child.exists(c => c.label == "scope" && (c.text == "provided" || c.text == "test")) =>
              val organization = e.child.filter(_.label == "groupId").flatMap(_.text).mkString
              val artifact = e.child.filter(_.label == "artifactId").flatMap(_.text).mkString
              val version = e.child.filter(_.label == "version").flatMap(_.text).mkString
              Comment(s"provided dependency $organization#$artifact;$version has been omitted")
            case _ => node
          }
        }).transform(node).head
      }
     */
    // skip dependency elements with a scope, e.g. "provided" + "test"
    pomPostProcess := {
      (node: XmlNode) =>
        new RuleTransformer(new RewriteRule {
          override def transform(node: XmlNode): XmlNodeSeq = node match {
            case e: Elem
                if e.label == "dependency"
                  && e.child.exists(child => child.label == "scope") =>
              def txt(label: String): String = "\"" + e.child.filter(_.label == label).flatMap(_.text).mkString + "\""
              Comment(s""" scoped dependency ${txt("groupId")} % ${txt("artifactId")} % ${txt("version")} % ${txt(
                "scope")} has been omitted """)
            case _ => node
          }
        }).transform(node).head
    },
    resolvers ++= Seq(
      DefaultMavenRepository,
      Resolver.mavenLocal,
      Resolver.sonatypeRepo("releases"),
      Resolver.sonatypeRepo("snapshots"),
      Resolver.typesafeRepo("releases"),
      Resolver.jcenterRepo
    )
  )

  // Metadata needed by Maven Central
  // See also http://maven.apache.org/pom.html#Developers
  import xerial.sbt.Sonatype.SonatypeKeys._
  import xerial.sbt.Sonatype._
  lazy val mavenCentralSettings = Seq(
    // if don't set sonatypeProfileName, it defaults to orgnaization...
    sonatypeProfileName := "com.memnets",
    publishMavenStyle := true,
    startYear := Some(2019),
    licenses := Seq("APL2" -> url("http://www.apache.org/licenses/LICENSE-2.0.html")),
    publishTo := sonatypePublishTo.value,
    sonatypeProjectHosting := Some(
      GitHubHosting("MemoryNetworks", "memnets", "K Jones", "kjones11235@users.noreply.github.com")
    )
    // GitHubHosting does homepage+developer...
    /*
      homepage := Some(url("https://github.com/MemoryNetworks/memnets")),
      pomExtra :=
          <developers>
            <developer>
              // <id></id>
              <name>K Jones</name>
              <url>https://github.com/MemoryNetworks</url>
            </developer>
          </developers>
   */
  )
  lazy val manifestSetting = packageOptions += {
    Package.ManifestAttributes(
      "Created-By" -> "Simple Build Tool",
      "Built-By" -> sys.env.getOrElse("JAR_BUILT_BY", sys.props("user.name")),
      "Build-Jdk" -> sys.props("java.version"),
      "Specification-Title" -> name.value,
      "Specification-Version" -> version.value,
      "Specification-Vendor" -> organization.value,
      "Implementation-Title" -> name.value,
      "Implementation-Version" -> version.value,
      "Implementation-Vendor-Id" -> organization.value,
      "Implementation-Vendor" -> organization.value
    )
  }
}
