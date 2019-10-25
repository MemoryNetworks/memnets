Common.settings

name := "memnets-lwjgl"

description := "MemNets LWJGL"

skip in publish := true

libraryDependencies ++= {
  val version = "3.2.2"
  val os = "windows" // TODO: Change to "linux" or "macos" if necessary
  Seq(
    "lwjgl",
    "lwjgl-glfw",
    "lwjgl-opengl"
    // TODO: Add more modules here
  ).flatMap { module =>
    {
      Seq(
        "org.lwjgl" % module % version,
        ("org.lwjgl" % module % version).classifier(s"natives-$os")
      )
    }
  }
}

// to use diff OS, refer to https://github.com/LWJGL/lwjgl3-demos/blob/master/pom.xml
libraryDependencies += "org.eclipse.swt" % "org.eclipse.swt.win32.win32.x86_64" % "4.6.1"
libraryDependencies += "org.joml" % "joml" % "1.9.15"

resolvers += "maven-eclipse-repo".at("http://maven-eclipse.github.io/maven")
