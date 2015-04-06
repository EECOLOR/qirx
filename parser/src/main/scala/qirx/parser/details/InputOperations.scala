package qirx.parser.details

import psp.api.Precise
import psp.api.InvariantView
import psp.api.HasPreciseSize
import psp.std._
import qirx.parser.Input

trait InputOperations {
  implicit class InputOperations(input: Input) {
    private val Input(underlying, position) = input

    def isEmpty  : Boolean  = underlying.isEmpty
    def nonEmpty : Boolean  = underlying.nonEmpty
    def head     : Char     = underlying.head
    def tail     : Input    = new Input(underlying.tail.to, position + 1)
    def size     : Precise  = underlying.size

    def mkString(separator: String): String = underlying mkString separator

    def takeWhile(p: ToBool[Char]): Input =
      new Input((underlying takeWhile p).to, position)

    def take(n: Precise): Input =
      new Input((underlying take n).to, position)

    def dropWhile(p: ToBool[Char]): Input = {
      val dropped: InvariantView[Char] with HasPreciseSize = (underlying dropWhile p).to
      new Input(dropped, position + (size.longValue - dropped.size.longValue))
    }

    def drop(n: Precise): Input =
      new Input((underlying drop n).to, position + n.longValue)

    def span(p: ToBool[Char]): SplitInput =
      SplitInput(takeWhile(p), dropWhile(p))

    def splitAt(index: Precise): SplitInput =
      SplitInput(take(index), drop(index))

  }
}
