# MemNets


MemNets is a framework for numerical simulations and games written in Scala, with Kotlin and Java friendly bindings. 

The latest release is 1.0.0, which is cross-built against 2.12, and 2.13
(not published to Maven Central yet.  coming soon)

## Example 

Building an oscillator using the sparse variable DSL:

```scala
  ModelBuilder("DSL sparse") {  b => import b._  // import brings in DSL implicits... 
	
    val freq = 0.5.toFreq()
    val dampen = 0.01

    val y = Y(name = "y")
    val x = Y(name = "x", decay = -dampen)  // y'' = -dampen * y'

    x --> y                             // y' = x
    y --> x w = -freq * freq  // y'' = - f^2 * y

    // can specify what the time chart shows on top or let the system guess
    track(y)
    
    system.onTick = { te =>
      if (te.modSec(2))
        logr.debug(f"y = ${y.act}%.02f at ${te.secs} sec")
    }

    Trial(name = "IC by Step", time = 1 m)
    // on = 0 sets initial condition (system forces duration = 0)
    Step(y = y, on = 0, scale = 10.0)

    Trial(name = "IC by onReset", time = 1 m)
      .onReset = { y.update(10.0) }

    Trial(name = "sin input", time = 1 m)
    Sin(y = y, on = 1 s, period = 0.5.toPeriod, phase = Math.PI, scale = 0.5)

  }
```
------------

### SBT

Add these lines (or only the ones you need) to your SBT project definition 
```scala
libraryDependencies  ++= Seq(
  "com.memnets" %% "api" % "1.0.0",
  "com.memnets" %% "models" % "1.0.0",
  "com.memnets" %% "awt" % "1.0.0",
  // for JavaFX
  "com.memnets" %% "fx" % "1.0.0"
)
```
(other jars are not published)

### Maven

```xml
<dependency>
  <groupId>com.memnets</groupId>
  <artifactId>memnets-api_2.12</artifactId>
  <version>1.0.0</version>
</dependency>
<dependency>
  <groupId>com.memnets</groupId>
  <artifactId>memnets-models_2.12</artifactId>
  <version>1.0.0</version>
</dependency>
...
```
### Project structure

- api 
core library is UI agnostic
- models 
numerous pre-built scientific models using only api in StandardLibrary.scala
if you would like to share a model, contribute it to CommunityLibrary.scala
- awt 
example app in Swing/AWT.
parts used by other two UI projects
- fx 
primary app development
can be built using JavaFX 8, 11, or 12
- lwjgl 
example app in SWT+OpenGL
jars not published

NOTE: most of the examples use the StandardLibrary, e.g., running LorentFX, JLorentz, or LorentzGL will all allow you to select other models

## Documentation

* https://github.com/MemoryNetworks/memnets/wiki

## Building/running yourself

- Install SBT 1.2.8+ (only version tested) 
- Clone this repository
- See Build.sbt for required environment variables and javafx options
- At root directory


```sbt
// NOTE: sbt may require more than the default memory here
sbt test // will download jars, compile, and run tests
sbt "project fx" "run"
```

(c) Memory Networks, 2019