package documentation

import java.nio.file.NoSuchFileException
import java.nio.file.Paths
import org.qirx.littlespec.Specification
import qirx.FailureHandler
import scala.concurrent.Await
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Promise
import scala.concurrent.duration.DurationInt

object _02_Overview_failures extends Specification {

"""|# An overview of the failures
   |
   |What could possible go wrong?
   |
   |In this part of the documentation we show you what problems we could have
   |encountered during the simple examples shown in the overview.""".stripMargin - {

  """|## Handling of failures
     |
     |The tools accept an implicit `FailureHandler`. If we point the tools towards
     |a directory that does not exist, we will receive a notification that it does
     |not exist.""".stripMargin - example {
       val failure = Promise[Throwable]
       val failureHandler = FailureHandler proxyTo failure.success

       new qirx.Qirx(Paths.get("./non-existing-directory"), failureHandler)

       Await.result(failure.future, 1.second) must beAnInstanceOf[NoSuchFileException]
     }
   }
}
