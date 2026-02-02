name := "EquivalenceFiddle"

import org.scalajs.linker.interface.OutputPatterns

name := "EquivalenceFiddle"
version := "0.1.0"

val scVersion = "2.13.13"

scalaVersion := scVersion

// Allow version eviction for scalajs-dom (needed for d3v4 compatibility)
ThisBuild / evictionErrorLevel := Level.Info

val scalacOpts = Seq(
  "-feature",
  "-language:implicitConversions",
  "-language:postfixOps",
  "-language:existentials",
  "-deprecation"
)

lazy val web = (project in file("web")).settings(
  scalaVersion := scVersion,
  scalaJSProjects := Seq(jsClient),
  Assets / pipelineStages := Seq(scalaJSPipeline),
  Compile / compile := ((Compile / compile) dependsOn (jsClient / Compile / fastOptJS / webpack)).value,
  Assets / unmanagedResources ++= (jsClient / Compile / fastOptJS / webpack).value.map(_.data),
  // Also include webpack bundle source map
  Assets / unmanagedResources += {
    val bundlePath = (jsClient / Compile / fastOptJS / webpack).value.head.data
    new File(bundlePath.getAbsolutePath + ".map")
  },
  // Include the intermediate Scala.js source map so the bundle map can reference it
  Assets / unmanagedResources += {
    val jsFile = (jsClient / Compile / fastOptJS).value.data
    new File(jsFile.getAbsolutePath + ".map")
  },
  // Include the intermediate Scala.js output and its source map for webpack to reference
  // Webpack's source-map-loader will include these in the bundle map
  Assets / unmanagedResources ++= {
    val jsFile = (jsClient / Compile / fastOptJS).value.data
    Seq(jsFile, new File(jsFile.getAbsolutePath + ".map"))
  },
    // Copy CSS files from bundler node_modules into managed assets (preserves lib/ paths)
    Assets / resourceGenerators += Def.task {
      val npmDir = (jsClient / Compile / fastOptJS).value.data.getParentFile / "node_modules"
      val outDir = (Assets / resourceManaged).value / "lib"

      val cmCss = npmDir / "codemirror" / "lib" / "codemirror.css"
      val bsCss = npmDir / "bootstrap" / "dist" / "css" / "bootstrap.min.css"

      val mappings = Seq(
        cmCss -> (outDir / "codemirror" / "lib" / "codemirror.css"),
        bsCss -> (outDir / "bootstrap" / "css" / "bootstrap.min.css")
      )

      mappings.flatMap { case (src, dest) =>
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
  // Copy Scala source files to web stage so source maps can reference them via HTTP
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
  scalaVersion := scVersion,
  name := "shared",
  scalacOptions ++= scalacOpts,
  assembly / test := {},
  libraryDependencies ++= Seq(
    "org.scalaz" %%% "scalaz-core" % "7.3.8",
    "org.scalactic" %% "scalactic" % "3.2.0",
    "org.scalatest" %% "scalatest" % "3.2.0" % "test"
  )
)

lazy val jsClient = (project in file("js-client")).settings(
  scalaVersion := scVersion,
  name := "eqfiddle-client",
  Compile / fastLinkJS / moduleName := "eqfiddle-client",
  ThisBuild / parallelExecution := false,
  scalacOptions ++= scalacOpts,
  scalacOptions += "-P:scalajs:nowarnGlobalExecutionContext",
  scalaJSLinkerConfig := {
    val baseConfig = scalaJSLinkerConfig.value
      .withModuleKind(ModuleKind.CommonJSModule)
      .withOutputPatterns(OutputPatterns.fromJSFile("eqfiddle-client.js"))
      .withSourceMap(true)
    // Use relative source map paths so the chain works when bundled and deployed
    baseConfig.withRelativizeSourceMapBase(Some(new java.net.URI(".")))
  },
  resolvers += "jitpack" at "https://jitpack.io",
  libraryDependencies ++= Seq(
    "org.scalaz" %%% "scalaz-core" % "7.3.8",
    "com.github.fdietze.scala-js-d3v4" %%% "scala-js-d3v4" % "ac063410c6e8e556fbd371fe5608cc12786aa9ab",
    "com.github.karasiq" %%% "scalajs-bootstrap" % "2.4.2",
    "org.scala-js" %%% "scalajs-dom" % "2.3.0"
  ),
  Compile / npmDependencies ++= Seq(
    "d3" -> "5.9.2",
    "jquery" -> "3.7.1",
    "bootstrap" -> "5.3.3",
    "codemirror" -> "5.13.0"
  ),
  Compile / npmResolutions ++= Map(
    "d3" -> "5.9.2",
    "jquery" -> "3.7.1",
    "bootstrap" -> "5.3.3"
  ),
  Compile / fastOptJS / webpackConfigFile := Some(baseDirectory.value / "webpack.config.js"),
  Compile / fullOptJS / webpackConfigFile := Some(baseDirectory.value / "webpack.config.js"),
  Compile / fastOptJS / webpackEmitSourceMaps := true,
  Compile / fullOptJS / webpackEmitSourceMaps := true,
  webpack / version := "5.88.2",
  webpackCliVersion := "5.1.4",
  startWebpackDevServer / version := "4.15.1",
  Compile / fastLinkJS / artifactPath :=
      ((Compile / classDirectory).value / "app" / ((fastLinkJS / moduleName).value + ".js")),
  Compile / fullOptJS / artifactPath := (Compile / fastLinkJS / artifactPath).value,
  Compile / unmanagedSourceDirectories +=
      baseDirectory.value / ".." / "shared" / "src" / "main" / "scala-2.13"
).aggregate(shared).dependsOn(shared).enablePlugins(ScalaJSPlugin, ScalaJSBundlerPlugin, ScalaJSWeb)

lazy val jsApi = (project in file("js-api")).settings(
  scalaVersion := scVersion,
  name := "eqfiddle-api",
  ThisBuild / parallelExecution := false,
  scalacOptions ++= scalacOpts,
  scalaJSLinkerConfig ~= { _.withModuleKind(ModuleKind.CommonJSModule).withOutputPatterns(OutputPatterns.fromJSFile("%s.js")) },
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
