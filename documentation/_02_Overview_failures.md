**This documentation is generated from `documentation._02_Overview_failures`**

---
# An overview of the failures

What could possible go wrong?

In this part of the documentation we show you what problems we could have
encountered during the simple examples shown in the overview.
## Handling of failures

The tools accept an implicit `FailureHandler`. If we point the tools towards
a directory that does not exist, we will receive a notification that it does
not exist.
```scala
val failure = Promise[Throwable]
val failureHandler = FailureHandler proxyTo failure.success

new qirx.Qirx(Paths.get("./non-existing-directory"), failureHandler)

Await.result(failure.future, 1.second) must beAnInstanceOf[NoSuchFileException]
```
