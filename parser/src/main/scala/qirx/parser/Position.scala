package qirx.parser

import scala.annotation.implicitNotFound
import psp.std.Long

case class Position(start: Long, end: Long)

object Position {
  object None extends Position(-1, -1)
}
