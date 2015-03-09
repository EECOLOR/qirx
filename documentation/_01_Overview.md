**This documentation is generated from `documentation._01_Overview`**

---
# Introduction

This library provides multiple features that seamlessly work together, it's
designed for programmers who wish to hook into their compiler at serveral
stages. As such this getting started document touches on the different
subjects to give you an overview of the possibilities.
Before we continue we need to make sure the temporary directory, used to
execute the examples in this documentation, is clear and exists.

Note that the asynchronous nature of the library requires us to define a
failure handler.

Before the documentation is run we define it. At the end of the documentation
we check if we have caught any failure.
```scala
val failurePromise = Promise[Throwable]
failure = failurePromise.future
failureHandler = FailureHandler proxyTo failurePromise.success
```
## Free

This section shows what the default implementation gives you for free, it's
nowhere near a complete description, but it should give you an overview.

Let's create the tools, point it to a directory and write a file to that
directory.

```scala
val defaultConfiguration = qirx.Qirx configurationWith failureHandler
val tools = new qirx.Qirx(tmpDirectory, defaultConfiguration)

val file = tmpDirectory.resolve("test.qirx")
Files.write(file, """println("hello world")""" getBytes UTF_8, CREATE_NEW)
```
The tools have picked up the creation of the file and started to work.

The first thing that happend was parsing the file into a more structured
format than a `String`. The result can be seen in a file that was created
next to it.

```scala
val rawFile = waitForCreationOf("test.qirx.raw")
val contents = new String(Files readAllBytes rawFile, UTF_8)
contents is """|Sequence(
  Id("println"),
  StringValue("hello world")
)""".stripMargin
```
Now we make sure that we did not mis any problems
```scala
Try(Await.result(failure, 1.second)).toOption match {
  case Some(failure) => throw failure
  case _             => success
}
```
