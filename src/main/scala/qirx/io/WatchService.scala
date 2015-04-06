package qirx.io

import java.nio.file.Path
import psp.std.Unit
import psp.std.?=>

trait WatchService {
  import WatchService.Event

  // TODO Change PartialFunction to Regular function and write a specification
  // that ensures partial functions are handled correctly
  def watch(dir: Path, listener: Event ?=> Unit):Unit
}

object WatchService {
  sealed trait Event {
    def file: Path
  }

  case class Create(file: Path) extends Event
}


