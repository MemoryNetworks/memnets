# The MemNets Framework

MemNets is a framework for numerical simulations and games written in Scala, with Kotlin and Java friendly bindings. 

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

The latest release is 1.0.0
   * JavaFX 8 (default) uses scala 2.12 
   * JavaFX 12 cross-built against scala 2.12 and 2.13 
   
*NOTE: currently only supports 2.13.0.  In 2.13.1, the same call to ScriptEngineManager that works in 2.12.x and 2.13.0 blows up*

*not published to Maven Central yet.  coming soon*

### SBT

Add these lines (or only the ones you need) to your SBT project definition 
```scala
libraryDependencies  ++= Seq(
  "com.memnets" %% "api" % "1.0.0",
  "com.memnets" %% "models" % "1.0.0",
  "com.memnets" %% "awt" % "1.0.0",
  // for JavaFX 8 (default)
  "com.memnets" %% "fx" % "1.0.0"
  // for JavaFX 12
  // "com.memnets" %% "fx" % "1.0.0-12.0.2"
)
```
*other jars are not published.  can use `sbt publishLocal`*

### Maven

Add these lines (or only the ones you need) to your project.  Append 2.12 or 2.13... 

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
<dependency>
  <groupId>com.memnets</groupId>
  <artifactId>memnets-awt_2.12</artifactId>
  <version>1.0.0</version>
</dependency>           
// JavaFX 8
<dependency>
  <groupId>com.memnets</groupId>
  <artifactId>memnets-fx_2.12</artifactId>
  <version>1.0.0</version>
</dependency>  
<!--
// JavaFX 12   
<dependency>
  <groupId>com.memnets</groupId>
  <artifactId>memnets-fx_2.12</artifactId>
  <version>1.0.0-12.0.2</version>
</dependency>
-->    

```

## Documentation

*  See [Project Wiki](https://github.com/MemoryNetworks/memnets/wiki)
*  See [Structure page](https://github.com/MemoryNetworks/memnets/wiki/Project-Structure) to determine which jars you need

## Building/running yourself

- Clone this repository
- See Build.sbt for required environment variables and javafx options

#### Use SBT

- Install SBT 1.2.8+ (only version tested)
- At install directory
 NOTE: sbt may require more than the default memory for compile+tests


```sbtshell
sbt test // will download jars, compile, and run tests
sbt "project fx" "run"
```

#### Use IntelliJ

(with Scala plugin installed/enabled)
1. New Project from Existing Sources
2. Point to install directory
3. Select Build.sbt
4. Wait 5-10 seconds for IDE to process build
5. Build fully imported project
6. Navigate to fx/src/main/scala/memnets.fx.demo/DemosFX.scala and right-click to run 

*NOTE: see the [Project Wiki](https://github.com/MemoryNetworks/memnets/wiki) for more on  IntelliJ* 

(c) Memory Networks, 2019