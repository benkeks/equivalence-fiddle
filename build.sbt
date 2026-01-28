name := "EquivalenceFiddle"

version := "0.1.0"

scalaVersion := "2.13.18"

val scalacOpts = Seq(
  "-Xmax-classfile-name", "140",
  "-feature",
  "-language:implicitConversions",
  "-language:postfixOps",
  "-language:existentials",
  "-deprecation"
)

lazy val web = (project in file("web")).settings(
  scalaVersion := "2.13.18",
  scalaJSProjects := Seq(jsClient),
  Assets / pipelineStages := Seq(scalaJSPipeline),
  Compile / compile := ((Compile / compile) dependsOn scalaJSPipeline).value,
  libraryDependencies ++= Seq(
    "org.webjars" % "codemirror" % "5.13",
    "org.webjars" % "jquery" % "2.1.3",
    "org.webjars" % "bootstrap" % "3.4.1"
  )
).enablePlugins(SbtWeb)

lazy val shared = (project in file("shared")).settings(
  scalaVersion := "2.13.18",
  name := "shared",
  scalacOptions ++= scalacOpts,
  assembly / test := {},
  libraryDependencies ++= Seq(
    "org.scalaz" %%% "scalaz-core" % "7.3.8",
    "org.scalactic" %% "scalactic" % "3.2.19",
    "org.scalatest" %% "scalatest" % "3.2.19" % "test"
  )
)

lazy val jsClient = (project in file("js-client")).settings(
  scalaVersion := "2.13.18",
  name := "eqfiddle-client",
  ThisBuild / parallelExecution := false,
  scalacOptions ++= scalacOpts,
  libraryDependencies ++= Seq(
    "org.scalaz" %%% "scalaz-core" % "7.3.8",
    "org.singlespaced" %%% "scalajs-d3" % "0.3.4",
    //"org.denigma" %%% "codemirror-facade" % "5.22.0-0.8", // now placed in js-client/lib
    "com.github.karasiq" %%% "scalajs-bootstrap" % "2.3.5"
  ),
  Compile / fastOptJS / artifactPath :=
      ((fastOptJS / target).value /
        ((fastOptJS / moduleName).value + ".js")),
  Compile / fullOptJS / artifactPath := (Compile / fastOptJS / artifactPath).value,
  Compile / unmanagedSourceDirectories +=
      baseDirectory.value / ".." / "shared" / "src" / "main" / "scala"
).aggregate(shared).dependsOn(shared).enablePlugins(ScalaJSPlugin, ScalaJSWeb)

lazy val jsApi = (project in file("js-api")).settings(
  scalaVersion := "2.13.18",
  name := "eqfiddle-api",
  ThisBuild / parallelExecution := false,
  scalacOptions ++= scalacOpts,
  scalaJSLinkerConfig ~= { _.withModuleKind(ModuleKind.CommonJSModule) },
  Compile / fastOptJS / artifactPath :=
      ((fastOptJS / target).value /
        ((fastOptJS / moduleName).value + ".js")),
  Compile / fullOptJS / artifactPath := (Compile / fastOptJS / artifactPath).value,
  Compile / unmanagedSourceDirectories +=
      baseDirectory.value / ".." / "shared" / "src" / "main" / "scala"
).aggregate(shared).dependsOn(shared).enablePlugins(ScalaJSPlugin)

lazy val root = project.in(file(".")).settings(
  name := "eqfiddle"
  ).aggregate(shared, jsClient, jsApi, web)
   .dependsOn(jsClient, web)
