name := "EquivalenceFiddle"

import org.scalajs.linker.interface.OutputPatterns

name := "EquivalenceFiddle"
version := "0.1.0"

val scVersion = "2.12.13"

scalaVersion := scVersion

// Allow version eviction for scalajs-dom (needed for d3v4 compatibility)
ThisBuild / evictionErrorLevel := Level.Info

val scalacOpts = Seq(
  "-Xmax-classfile-name", "140",
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
  Compile / compile := ((Compile / compile) dependsOn scalaJSPipeline dependsOn (jsClient / Compile / packageJSDependencies)).value,
  Assets / unmanagedResources += (jsClient / Compile / fastLinkJS / artifactPath).value,
  Assets / unmanagedResources += (jsClient / Compile / packageJSDependencies / artifactPath).value
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
  scalaJSLinkerConfig := {
    scalaJSLinkerConfig.value
      .withModuleKind(ModuleKind.ESModule)
      .withOutputPatterns(OutputPatterns.fromJSFile("eqfiddle-client.js"))
  },
  resolvers += "jitpack" at "https://jitpack.io",
  libraryDependencies ++= Seq(
    "org.scalaz" %%% "scalaz-core" % "7.3.8",
    "com.github.fdietze.scala-js-d3v4" %%% "scala-js-d3v4" % "ac063410c6e8e556fbd371fe5608cc12786aa9ab",
    "com.github.karasiq" %%% "scalajs-bootstrap" % "2.4.2",
    "org.scala-js" %%% "scalajs-dom" % "1.0.0"
  ),
  dependencyOverrides += "org.scala-js" %%% "scalajs-dom" % "1.0.0",
  Compile / fastLinkJS / artifactPath :=
      ((Compile / classDirectory).value / "app" / ((fastLinkJS / moduleName).value + ".js")),
  Compile / fullOptJS / artifactPath := (Compile / fastLinkJS / artifactPath).value,
  jsDependencies ++= Seq(
    "org.webjars" % "d3js" % "5.9.1" / "d3.min.js",
    "org.webjars" % "codemirror" % "5.13" / "codemirror.js",
    "org.webjars" % "jquery" % "2.1.3" / "jquery.js",
    "org.webjars" % "bootstrap" % "3.4.1" / "bootstrap.min.js"
  ),
  packageJSDependencies / skip := false,
  Compile / packageJSDependencies / artifactPath := ((Compile / classDirectory).value / "app" / ((fastLinkJS / moduleName).value + "-jsdeps.js")),
  Global / excludeLintKeys += Compile / packageJSDependencies / artifactPath,
  Compile / unmanagedSourceDirectories +=
      baseDirectory.value / ".." / "shared" / "src" / "main" / "scala-2.12"
).aggregate(shared).dependsOn(shared).enablePlugins(ScalaJSPlugin, ScalaJSWeb, JSDependenciesPlugin)

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
      baseDirectory.value / ".." / "shared" / "src" / "main" / "scala-2.12"
).aggregate(shared).dependsOn(shared).enablePlugins(ScalaJSPlugin)

lazy val root = project.in(file(".")).settings(
  name := "eqfiddle"
  ).aggregate(shared, jsClient, jsApi, web)
   .dependsOn(jsClient, web)
