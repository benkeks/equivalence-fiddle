name := "EquivalenceFiddle"

version := "0.1.0"

scalaVersion := "2.12.10"

val scalacOpts = Seq(
  "-Xmax-classfile-name", "140",
  "-feature",
  "-language:implicitConversions",
  "-language:postfixOps",
  "-language:existentials",
  "-deprecation"
)

lazy val web = (project in file("web")).settings(
  scalaVersion := "2.12.10",
  scalaJSProjects := Seq(jsClient),
  isDevMode in scalaJSPipeline := true,
  pipelineStages in Assets := Seq(scalaJSPipeline),
  compile in Compile := ((compile in Compile) dependsOn scalaJSPipeline).value,
  libraryDependencies ++= Seq(
    "org.webjars" % "codemirror" % "5.13",
    "org.webjars" % "jquery" % "2.1.3",
    "org.webjars" % "bootstrap" % "3.4.1"
  )
).enablePlugins(SbtWeb)

lazy val shared = (project in file("shared")).settings(
  scalaVersion := "2.12.10",
  name := "shared",
  scalacOptions ++= scalacOpts,
  test in assembly := {},
  libraryDependencies ++= Seq(
    "org.scalaz" %%% "scalaz-core" % "7.2.29",
    "org.scalactic" %% "scalactic" % "3.2.0",
    "org.scalatest" %% "scalatest" % "3.2.0" % "test"
  )
)

lazy val jsClient = (project in file("js-client")).settings(
  scalaVersion := "2.12.10",
  name := "coupledsim-client",
  parallelExecution in ThisBuild := false,
  scalacOptions ++= scalacOpts,
  resolvers += sbt.Resolver.bintrayRepo("denigma", "denigma-releases"),
  libraryDependencies ++= Seq(
    "org.scalaz" %%% "scalaz-core" % "7.2.29",
    "org.singlespaced" %%% "scalajs-d3" % "0.3.4",
    "org.denigma" %%% "codemirror-facade" % "5.22.0-0.8",
    "com.github.karasiq" %%% "scalajs-bootstrap" % "2.3.5"
  ),
  artifactPath in (Compile,fastOptJS) :=
      ((target in fastOptJS).value /
        ((moduleName in fastOptJS).value + ".js")),
  artifactPath in (Compile,fullOptJS) := (artifactPath in (Compile,fastOptJS)).value,
  jsDependencies ++= Seq(
    "org.webjars" % "codemirror" % "5.13" / "codemirror.js",
    "org.webjars" % "jquery" % "2.1.3" / "2.1.3/jquery.js",
    "org.webjars" % "bootstrap" % "3.4.1" / "bootstrap.min.js"
  ),
  unmanagedSourceDirectories in Compile +=
      baseDirectory.value / ".." / "shared" / "src" / "main" / "scala-2.12"
).aggregate(shared).dependsOn(shared).enablePlugins(ScalaJSPlugin, ScalaJSWeb)

lazy val root = project.in(file(".")).settings(
  name := "coupledsim"
  ).aggregate(shared, jsClient, web)
   .dependsOn(jsClient, web)
