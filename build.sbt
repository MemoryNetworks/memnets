/**
 * NOTES: built using sbt 1.2.8
 *
 * for compile, you must have these SYSTEM/ENV VARIABLEs set (examples for windows):
 * 1. JAVA_HOME set to JDK root dir (e.g., "C:\dev\tools\openjdk-12.0.2")
 * 2. PATH entry to JDK/bin (e.g., "%JAVA_HOME%\bin")
 *
 * for runtime, if using javafx11+/openjfx11+, then must have SYSTEM/ENV VAR set :
 * 1. JAVAFX_LIB set to JFX lib dir (e.g., "C:\dev\tools\javafx-sdk-12.0.2\lib")
 *
 * check top of sbt println info to make sure paths match expected
 *
 * Common.settings are in \project\Build.scala
 * to change JavaFX version, set Common.FxVersion = fx12 or fx8 or fx11 in Build.scala
 *
 * an unneeded (empty) memnets-root.jar will be built
 *
 * tested using:
 *
 * scala 2.12.9
 * jdk 1.8.0_221
 * oracle jdk and openjdk with openjfx 11.0.2
 * oracle jdk and openjdk with openjfx 12.0.2
 *
 * NOTE: when using java11+, sbt omits some strange errors that don't actually break build
 */
Common.settings

name := "memnets-root"

skip in publish := true

lazy val root = project
  .in(file("."))
  .aggregate(api, models, awt, fx, lwjgl, drools, cuda, gamer, pro, research)
  .dependsOn(api, models, awt, fx, lwjgl, drools, cuda, gamer, pro, research)
//.aggregate(api, models, awt, fx, lwjgl)
//.dependsOn(api, models, awt, fx, lwjgl)

lazy val api = project.in(file("api"))

lazy val models = project.in(file("models")).dependsOn(api % "compile;test->test")

lazy val awt = project.in(file("awt")).dependsOn(api % "compile;test->test", models)

lazy val fx = project.in(file("fx")).dependsOn(api % "compile;test->test", models, awt)

lazy val kotlin = project.in(file("kotlin")).dependsOn(api, models, awt, fx)

// optional project to prototype OpenGL.
// if comment block out, need to remove from root.dependsOn and .aggregate
lazy val lwjgl = project.in(file("lwjgl")).dependsOn(api, models, awt)

/**
 * Below is not community edition and can be ignore if accidentally included
 */
lazy val gamer = project.in(file("gamer")).dependsOn(api, models, awt, fx)

lazy val drools = project.in(file("drools")).dependsOn(api % "compile;test->test")

lazy val cuda = project.in(file("cuda")).dependsOn(api % "compile;test->test")

lazy val pro = project
  .in(file("pro"))
  .dependsOn(api % "compile;test->test", models, awt, fx, gamer, cuda, drools)

lazy val research = project
  .in(file("research"))
  .dependsOn(api % "compile;test->test", models, awt, fx, lwjgl, gamer, cuda, drools, pro)
