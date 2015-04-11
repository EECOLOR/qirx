package qirx.parser
package parsers

import psp.api.HasPreciseSize
import psp.api.InvariantView
import psp.api.View
import psp.std.Char
import psp.std.String
import psp.std.implicitBuildsString
import psp.std.upcastForView
import qirx.parser.details.SplitInput

case class CharacterParser[A](
  consume: Input => SplitInput,
  toValue: InvariantView[Char] with HasPreciseSize => A
) extends Parser[A] {

  def parse(input: Input): Failure | View[Result[A]] =
    if (input.isEmpty) failure(ExpectedInput(input))
    else {
      val result = consume(input)
      if (result.consumed.nonEmpty) success(result, toValue)
      else failure(InvalidInput(input))
    }
}

object CharacterParser {

  def string[A](value: View[Char] with HasPreciseSize, toValue: String => A): CharacterParser[A] = {
    val size = value.size
    CharacterParser(
      consume = { input =>
        val consumed = input.take(size)
        if (consumed.underlying.force == value.force) SplitInput(consumed, input.drop(size))
        else SplitInput(Input.empty, input)
      },
      toValue = toValue compose (_.force)
    )
  }
}
