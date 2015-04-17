package qirx.parser
package parsers

import psp.api._
import psp.std._
import qirx.parser.Failure
import qirx.parser.details.Outcome
import qirx.parser.details.Consumed
import qirx.parser.details.Rejected

case class CharacterParser[A](
  consume: Input => Outcome,
  toValue: InvariantView[Char] with HasPreciseSize => A
) extends Parser[A] {

  def parse(input: Input): Failure | View[Result[A]] =
    if (input.isEmpty) failure(ExpectedInput(input))
    else {
      consume(input).fold(
        ifConsumed = success(_, _, toValue),
        ifRejected = message => failure(InvalidInput(input, message))
      )
    }
}

object CharacterParser {

  def string[A](value: View[Char] with HasPreciseSize, toValue: String => A): CharacterParser[A] = {
    val size = value.size
    val expected = value.force
    CharacterParser(
      consume = { input =>
        val consumed = input.take(size)
        if (consumed.underlying.force == expected) Consumed(consumed, input.drop(size))
        else Rejected(s"Expected `${value.force}`, got `${consumed.underlying.force[String]}`")
      },
      toValue = toValue compose (_.force)
    )
  }
}
