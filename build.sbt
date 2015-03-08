val settings = Seq(
          name := "qirx",
  organization := "org.qirx",
  scalaVersion := "2.11.6"
)

val testDependencies = Seq(
  libraryDependencies += "org.qirx" %% "little-spec" % "0.4" % "test",
       testFrameworks += new TestFramework("org.qirx.littlespec.sbt.TestFramework")
)

lazy val `qirx` = project.in( file(".") )
  .settings(settings ++ testDependencies : _*)
  .settings(
    noJavaSourceIn(Compile),
    noJavaSourceIn(Test)
  )

def noJavaSourceIn(configuration:Configuration) =
  unmanagedSourceDirectories in configuration := Seq((scalaSource in configuration).value)
