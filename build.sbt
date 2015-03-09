val settings = Seq(
          name := "qirx",
  organization := "org.qirx",
  scalaVersion := "2.11.6",
  fork in Test := true
)

libraryDependencies ++= Seq(
  "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.3" % "provided",
  "com.codecommit"         %% "gll-combinators"          % "2.2"   % "provided",
  "org.parboiled"          %% "parboiled"                % "2.1.0" % "provided"
)

val testSettings = Seq(
  libraryDependencies += "org.qirx" %% "little-spec"                     % "0.4" % "test",
  libraryDependencies += "org.qirx" %% "little-spec-extra-documentation" % "0.4" % "test",
       testFrameworks += new TestFramework("org.qirx.littlespec.sbt.TestFramework"),
          testOptions += Tests.Argument("reporter", "org.qirx.littlespec.reporter.MarkdownReporter"),
          testOptions += Tests.Argument("documentationTarget", ((baseDirectory in ThisBuild).value / "documentation").getAbsolutePath)
)

lazy val `qirx` = project.in( file(".") )
  .settings(settings ++ testSettings : _*)
  .settings(
    noJavaSourceIn(Compile),
    noJavaSourceIn(Test)
  )

def noJavaSourceIn(configuration:Configuration) =
  unmanagedSourceDirectories in configuration := Seq((scalaSource in configuration).value)
