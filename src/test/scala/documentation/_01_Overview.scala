package documentation

import java.io.IOException
import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.FileVisitResult
import java.nio.file.Files
import java.nio.file.Files.createDirectories
import java.nio.file.Files.exists
import java.nio.file.Path
import java.nio.file.Paths
import java.nio.file.SimpleFileVisitor
import java.nio.file.StandardOpenOption.CREATE_NEW
import java.nio.file.attribute.BasicFileAttributes
import org.qirx.littlespec.Specification
import org.qirx.littlespec.macros.Location
import qirx.FailureHandler
import qirx.io.DefaultWatchService
import qirx.io.WatchService.Create
import scala.concurrent.Await
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.concurrent.Promise
import scala.concurrent.duration.DurationInt
import scala.util.Try
import psp.std._

class _01_Overview extends Specification {
  /* required for the location macro of little-spec */
  protected[this] val Seq = psp.std.scSeq

  val tmpDirectory = Paths.get("./tmp/_01_Overview")

"""|# Introduction
   |
   |This library provides multiple features that seamlessly work together, it's
   |designed for programmers who wish to hook into their compiler at serveral
   |stages. As such this getting started document touches on the different
   |subjects to give you an overview of the possibilities.""".stripMargin - {

  """|Before we continue we need to make sure the temporary directory, used to
     |execute the examples in this documentation, is clear and exists.
     |""".stripMargin - {
       clear(tmpDirectory)
       exists(tmpDirectory) is false
       createDirectories(tmpDirectory)
       exists(tmpDirectory) is true
     }

  """|Note that the asynchronous nature of the library requires us to define a
     |failure handler.
     |""".stripMargin - {
       implicit var failureHandler: FailureHandler = null
       var failure: Future[Throwable] = null

    """|Before the documentation is run we define it. At the end of the documentation
       |we check if we have caught any failure.""".stripMargin - sideEffectExample {
         val failurePromise = Promise[Throwable]
         failure = failurePromise.future
         failureHandler = FailureHandler proxyTo failurePromise.success
       }

    """|## Free
       |
       |This section shows what the default implementation gives you for free, it's
       |nowhere near a complete description, but it should give you an overview.
       |
       |Let's create the tools, point it to a directory and write a file to that
       |directory.
       |""".stripMargin - sideEffectExample {
         val defaultConfiguration = qirx.Qirx configurationWith failureHandler
         val tools = new qirx.Qirx(tmpDirectory, defaultConfiguration)

         val file = tmpDirectory.resolve("test.qirx")
         Files.write(file, """println("hello world")""" getBytes UTF_8, CREATE_NEW)
       }

    """|The tools have picked up the creation of the file and started to work.
       |
       |The first thing that happend was parsing the file into a more structured
       |format than a `String`. The result can be seen in a file that was created
       |next to it.
       |""".stripMargin - example {
         val rawFile = waitForCreationOf("test.qirx.raw")
         val contents = new String(Files readAllBytes rawFile, UTF_8)
         contents is """|Sequence(
                        |  Id("println"),
                        |  StringValue("hello world")
                        |)""".stripMargin
       }

      "Now we make sure that we did not mis any problems" - example {
        Try(Await.result(failure, 1.second)).toOption match {
          case Some(failure) => throw failure
          case _             => success
        }
      }
    }
  }

  private[this] def waitForCreationOf(name:String)(implicit failureHandler: FailureHandler) = {
    val filePromise = Promise[Path]
    new DefaultWatchService(failureHandler).watch(tmpDirectory, {
      case Create(file) if file.toString endsWith name  =>
        filePromise.success(file)
    })
    Await.result(filePromise.future, 2.seconds)
  }

  private[this] def clear(dir:Path):Unit =
    if (Files exists dir) Files.walkFileTree(dir, Delete)

  private[this] object Delete extends SimpleFileVisitor[Path] {
    override def visitFile(file: Path, attrs: BasicFileAttributes) = {
      Files.delete(file)
      FileVisitResult.CONTINUE
    }
    override def postVisitDirectory(dir: Path, exception: IOException) =
      if (exception == null) {
        Files.delete(dir)
        FileVisitResult.CONTINUE
      } else throw exception
  }

  private[this] def sideEffectExample[T](code: => T)(implicit location: Location) =
    example { code ; success }
}
