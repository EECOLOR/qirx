lazy val `qirx` = project.in( file(".") )
  .aggregate(`qirx-grammar`)

lazy val `qirx-grammar` = project.in( file("grammar") )
  .settings(commonSettings ++ testSettings : _*)


//////// Common ////////

lazy val commonSettings = Seq(
  organization := "org.qirx",
  scalaVersion := "2.11.6"
) ++ noJavaSourceSettings ++ standardLibrarySettings

lazy val noJavaSourceSettings = Seq(
  noJavaSourceIn(Compile),
  noJavaSourceIn(Test)
)

def noJavaSourceIn(configuration:Configuration) =
  unmanagedSourceDirectories in configuration := Seq((scalaSource in configuration).value)

lazy val standardLibrarySettings = Seq(
            resolvers +=  "bintray/paulp" at "https://dl.bintray.com/paulp/maven",
  libraryDependencies +=  "org.improving" %% "psp-std" % "0.5.5",
        scalacOptions += "-Yno-predef",
        scalacOptions += "-Yno-imports"
)


////////  Test  ////////

lazy val testSettings = Seq(
         fork in Test := true,
  libraryDependencies += "org.qirx" %% "little-spec"                     % "0.4" % "test",
  libraryDependencies += "org.qirx" %% "little-spec-extra-documentation" % "0.4" % "test",
       testFrameworks += new TestFramework("org.qirx.littlespec.sbt.TestFramework"),
          testOptions += Tests.Argument("reporter", "org.qirx.littlespec.reporter.MarkdownReporter"),
          testOptions += Tests.Argument("documentationTarget", documentationTarget((baseDirectory in ThisProject).value))
)

def documentationTarget(base:File) = (base / "documentation").getAbsolutePath
