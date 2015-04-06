package qirx.parser

import psp.api.InvariantView
import psp.api.HasPreciseSize
import psp.api.Direct
import psp.std._
import qirx.parser.details.InputOperations

case class Input(
  underlying: InvariantView[Char] with HasPreciseSize,
  position: Long
)

object Input extends InputOperations {
  def empty: Input = new Input(emptyValue[Direct[Char]], 0L)

  import scala.language.implicitConversions
  implicit def fromString(string: String):Input = new Input(string, 0L)
}
