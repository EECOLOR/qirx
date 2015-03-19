package qirx.parser
package parsers

import psp.api._
import psp.std.{ Failure => _, _ }

case class CharacterParser[A](
  consume: Input => SplitInput,
  toValue: InvariantView[Char] with HasPreciseSize => A
) extends Parser[A] {

  def parse(input: Input): Failure | View[Result[A]] =
    if (input.isEmpty) failure(ExpectedInput(input))
    else {
      val SplitInput(consumed, remaining) = consume(input)
      if (consumed.isEmpty) failure(InvalidInput(input))
      else success(toValue(consumed.underlying), remaining)
    }
}

object CharacterParser {

  def string(value: View[Char] with HasPreciseSize): CharacterParser[String] = {
    val size = value.size
    CharacterParser(
      consume = { input =>
        val consumed = input.take(size)
        if (consumed.underlying.force == value.force) SplitInput(consumed, input.drop(size))
        else SplitInput(Input.empty, input)
      },
      toValue = _.force
    )
  }

  def char(value: Char): CharacterParser[Char] = {
    CharacterParser(
      consume = _.span(_ == value),
      toValue = _.head
    )
  }
}
