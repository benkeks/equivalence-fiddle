name := "EquivalenceFiddle"

version := "0.1.0"

scalaVersion := "2.12.15"

val scalacOpts = Seq(
  "-Xmax-classfile-name", "140",
  "-feature",
  "-language:implicitConversions",
  "-language:postfixOps",
  "-language:existentials",
  "-deprecation"
)

lazy val web = (project in file("web")).settings(
  scalaVersion := "2.12.15",
  scalaJSProjects := Seq(jsClient),
  Assets / pipelineStages := Seq(scalaJSPipeline),
  Compile / compile := ((Compile / compile) dependsOn scalaJSPipeline).value,
  // packageJSDependencies / skip := false,
  // libraryDependencies ++= Seq(
  //   "org.webjars" % "codemirror" % "5.13",
  //   "org.webjars" % "jquery" % "2.1.3",
  //   "org.webjars" % "bootstrap" % "3.4.1"
  // )
).enablePlugins(SbtWeb)

lazy val shared = (project in file("shared")).settings(
  scalaVersion := "2.12.15",
  name := "shared",
  scalacOptions ++= scalacOpts,
  assembly / test := {},
  libraryDependencies ++= Seq(
    "org.scalaz" %%% "scalaz-core" % "7.3.6",
    "org.scalactic" %% "scalactic" % "3.2.0",
    "org.scalatest" %% "scalatest" % "3.2.0" % "test"
  )
)

lazy val jsClient = (project in file("js-client")).settings(
  scalaVersion := "2.12.15",
  name := "eqfiddle-client",
  ThisBuild / parallelExecution := false,
  scalacOptions ++= scalacOpts,
  libraryDependencies ++= Seq(
    //"org.scalaz" %%% "scalaz-core" % "7.2.29",
    //"org.singlespaced" %%% "scalajs-d3" % "0.3.4",
    //"org.denigma" %%% "codemirror-facade" % "5.22.0-0.8", // now placed in js-client/lib
    "com.github.karasiq" %%% "scalajs-bootstrap" % "2.4.1",
    "io.udash" %%% "udash-jquery" % "3.0.4"
  ),
  Compile / fastOptJS / artifactPath :=
      ((fastOptJS / target).value /
        ((fastOptJS / moduleName).value + ".js")),
  Compile / fullOptJS / artifactPath := (Compile / fastOptJS / artifactPath).value,
  packageJSDependencies / skip := false,
  Compile / npmDependencies ++= Seq(
    "d3" -> "7.1.0",
    "@types/d3" -> "7.1.0"
  ),
  jsDependencies ++= Seq(
    "org.webjars" % "codemirror" % "5.13" / "codemirror.js",
    "org.webjars" % "jquery" % "2.1.4" / "2.1.4/jquery.js",
    "org.webjars" % "bootstrap" % "3.4.1" / "bootstrap.min.js"
  ),
  Compile / unmanagedSourceDirectories +=
      baseDirectory.value / ".." / "shared" / "src" / "main" / "scala-2.12"
).aggregate(shared).dependsOn(shared).enablePlugins(ScalaJSPlugin, ScalablyTypedConverterPlugin, JSDependenciesPlugin, ScalaJSWeb)

lazy val root = project.in(file(".")).settings(
  name := "eqfiddle"
  ).aggregate(shared, jsClient, web)
   .dependsOn(jsClient, web)
