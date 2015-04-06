package qirx.io

import java.nio.file.Path
import java.nio.file.StandardWatchEventKinds.ENTRY_CREATE
import java.nio.file.WatchKey
import java.nio.file.{WatchService => JavaWatchService}
import qirx.FailureHandler
import qirx.io.WatchService.Create
import qirx.io.WatchService.Event
import scala.collection.JavaConverters._
import scala.concurrent.ExecutionContext
import scala.concurrent.Future
import scala.concurrent.blocking
import psp.std.Unit
import psp.std.?=>
import psp.std.abort

class DefaultWatchService(failureHandler: FailureHandler)(implicit ec: ExecutionContext) extends WatchService {

  def watch(dir: Path, listener: Event ?=> Unit):Unit =
    Future(watchUsing(dir, listener)).failed foreach failureHandler.failure

  /*
    Don't get me started on the problems with the watch service
    interface. There are so many hidden dependencies. In the
    documentation: "Reset the key -- this step is critical if
    you want to receive further watch events."
    Well what do you think! That I don't? If I don't want to
    receive more events, I'll let you know.

    ...

    Anyway, the handling of this service is minimal and does not
    catch potential problems. If there are any, we need to create
    tests to expose those.
  */

  private[this] def watchServiceFor(dir: Path) = {
    val watchService = dir.getFileSystem.newWatchService
    dir.register(watchService, ENTRY_CREATE)
    watchService
  }

  private[this] def watchUsing(dir: Path, handler: Event ?=> Unit) =
    withKeyFrom(watchServiceFor(dir)) {
      _.pollEvents.asScala.foreach { javaEvent =>
        val detachedPath = javaEvent.context.asInstanceOf[Path]
        val event = Create(dir resolve detachedPath)
        if (handler isDefinedAt event) handler apply event
      }
    }

  private[this] def withKeyFrom(watchService :JavaWatchService)(action: WatchKey => Unit) = {
    blocking {
      var valid = true
      while (valid) {
        val key = watchService.take
        action(key)
        valid = key.reset
      }
      abort("Please file a ticket and create a test to add a more sensible way of handling this")
    }
  }
}
