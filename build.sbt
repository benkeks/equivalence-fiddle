import org.scalajs.linker.interface.OutputPatterns

name := "equiv.io"
version := "0.4.0"

val scVersion = "2.13.18"

scalaVersion := scVersion

val scalacOpts = Seq(
  "-feature",
  "-language:implicitConversions",
  "-language:postfixOps",
  "-language:existentials",
  "-deprecation"
)

// Common settings for Scala.js projects
lazy val commonSettings = Seq(
  scalaVersion := scVersion,
  scalacOptions ++= scalacOpts
)

lazy val web = (project in file("web")).settings(
  commonSettings,
  scalaJSProjects := Seq(jsClient),
  Assets / pipelineStages := Seq(scalaJSPipeline),
  Compile / compile := ((Compile / compile) dependsOn (jsClient / Compile / fastOptJS / webpack)).value,
  // Bundle webpack outputs and source maps as assets
  Assets / unmanagedResources ++= {
    val bundle = (jsClient / Compile / fastOptJS / webpack).value.head.data
    val bundleMap = new File(bundle.getAbsolutePath + ".map")
    val jsFile = (jsClient / Compile / fastOptJS).value.data
    val jsMap = new File(jsFile.getAbsolutePath + ".map")
    Seq(bundle, bundleMap, jsFile, jsMap)
  },
    // Copy CSS files from bundler node_modules into managed assets
    Assets / resourceGenerators += Def.task {
      val npmDir = (jsClient / Compile / fastOptJS).value.data.getParentFile / "node_modules"
      val outDir = (Assets / resourceManaged).value / "lib"

      val cssFiles = Seq(
        npmDir / "codemirror" / "lib" / "codemirror.css" -> outDir / "codemirror" / "lib" / "codemirror.css",
        npmDir / "bootstrap" / "dist" / "css" / "bootstrap.min.css" -> outDir / "bootstrap" / "css" / "bootstrap.min.css"
      )

      cssFiles.flatMap { case (src, dest) =>
        if (src.exists) {
          IO.createDirectory(dest.getParentFile)
          IO.copyFile(src, dest)
          Seq(dest)
        } else Seq.empty
      }
    }.taskValue,
  // Ensure CSS are included in assets
  Assets / unmanagedResources ++= {
    val libDir = baseDirectory.value / "src" / "main" / "assets" / "lib"
    (libDir.globRecursive("*.css").get ++ libDir.globRecursive("*.min.css").get).distinct
  },
  // Copy Scala source files to web stage for source map resolution
  Assets / resourceGenerators += Def.task {
    val sharedSrcDir = (jsClient / baseDirectory).value / ".." / "shared" / "src" / "main" / "scala-2.13"
    val jsSrcDir = (jsClient / baseDirectory).value / "src" / "main" / "scala-2.13"
    val outDir = (Assets / resourceManaged).value / "scala"
    
    val sharedFiles = sharedSrcDir.globRecursive("*.scala").get
    val jsFiles = jsSrcDir.globRecursive("*.scala").get
    
    // Also look for Scala source files in the bundler temp directory (includes hash directory structure)
    val bundlerDir = (jsClient / Compile / fastOptJS).value.data.getParentFile
    val bundlerSrcFiles = (bundlerDir / "scala").globRecursive("*.scala").get
    
    (sharedFiles ++ jsFiles ++ bundlerSrcFiles).flatMap { srcFile =>
      // Preserve the full relative path including hash directories
      val relativePath = if (srcFile.toString.contains("scala")) {
        // For bundler files, keep the path after "scala/" (e.g., "0c915f/io/equiv/...")
        val parts = srcFile.toString.split("scala" + java.io.File.separator)
        if (parts.length > 1) parts(1) else srcFile.getName
      } else if (srcFile.toString.contains("shared/src/main/scala-2.13")) {
        IO.relativize(sharedSrcDir, srcFile).getOrElse(srcFile.getName)
      } else {
        IO.relativize(jsSrcDir, srcFile).getOrElse(srcFile.getName)
      }
      val destFile = outDir / relativePath
      IO.createDirectory(destFile.getParentFile)
      IO.copyFile(srcFile, destFile)
      Seq(destFile)
    }
  }.taskValue
).enablePlugins(SbtWeb)

lazy val shared = (project in file("shared")).settings(
  commonSettings,
  name := "shared",
  assembly / test := {},
  libraryDependencies ++= Seq(
    "org.scalaz" %%% "scalaz-core" % "7.3.8",
    "org.scalactic" %% "scalactic" % "3.2.19",
    "org.scalatest" %% "scalatest" % "3.2.19" % "test"
  )
)

lazy val jsClient = (project in file("js-client")).settings(
  commonSettings,
  name := "eqfiddle-client",
  Compile / fastLinkJS / moduleName := "eqfiddle-client",
  ThisBuild / parallelExecution := false,
  scalacOptions += "-P:scalajs:nowarnGlobalExecutionContext",
  scalaJSLinkerConfig := {
    scalaJSLinkerConfig.value
      .withModuleKind(ModuleKind.CommonJSModule)
      .withOutputPatterns(OutputPatterns.fromJSFile("eqfiddle-client.js"))
      .withSourceMap(true)
      .withRelativizeSourceMapBase(Some(new java.net.URI(".")))
  },
  resolvers += "jitpack" at "https://jitpack.io",
  libraryDependencies ++= Seq(
    "org.scalaz" %%% "scalaz-core" % "7.3.8",
    "com.github.fdietze.scala-js-d3v4" %%% "scala-js-d3v4" % "d5f63d1849",
    "org.scala-js" %%% "scalajs-dom" % "2.8.1"
  ),
  Compile / npmDependencies ++= Seq(
    "d3" -> "5.16.0",
    "jquery" -> "3.7.1",
    "bootstrap" -> " 5.3.8",
    "codemirror" -> "5.65.20"
  ),
  Compile / npmResolutions ++= Map(
    "d3" -> "5.16.0",
    "jquery" -> "3.7.1"
  ),
  Compile / fastOptJS / webpackConfigFile := Some(baseDirectory.value / "webpack.config.js"),
  Compile / fullOptJS / webpackConfigFile := Some(baseDirectory.value / "webpack.config.js"),
  Compile / fastOptJS / webpackEmitSourceMaps := true,
  Compile / fullOptJS / webpackEmitSourceMaps := true,
  webpack / version := "5.105.0",
  webpackCliVersion := "5.1.4",
  startWebpackDevServer / version := "5.2.3",
  Compile / fastLinkJS / artifactPath :=
      ((Compile / classDirectory).value / "app" / ((fastLinkJS / moduleName).value + ".js")),
  Compile / fullOptJS / artifactPath := (Compile / fastLinkJS / artifactPath).value,
  Compile / unmanagedSourceDirectories +=
      baseDirectory.value / ".." / "shared" / "src" / "main" / "scala-2.13"
).aggregate(shared).dependsOn(shared).enablePlugins(ScalaJSPlugin, ScalaJSBundlerPlugin, ScalaJSWeb)

lazy val jsApi = (project in file("js-api")).settings(
  commonSettings,
  name := "eqfiddle-api",
  ThisBuild / parallelExecution := false,
  scalaJSLinkerConfig ~= { 
    _.withModuleKind(ModuleKind.CommonJSModule)
     .withOutputPatterns(OutputPatterns.fromJSFile("%s.js")) 
  },
  Compile / fastLinkJS / artifactPath :=
      ((fastLinkJS / target).value /
        ((fastLinkJS / moduleName).value + ".js")),
  Compile / fullOptJS / artifactPath := (Compile / fastLinkJS / artifactPath).value,
  Compile / unmanagedSourceDirectories +=
      baseDirectory.value / ".." / "shared" / "src" / "main" / "scala-2.13"
).aggregate(shared).dependsOn(shared).enablePlugins(ScalaJSPlugin)

lazy val root = project.in(file(".")).settings(
  name := "eqfiddle"
).aggregate(shared, jsClient, jsApi, web)
 .dependsOn(jsClient, web)
