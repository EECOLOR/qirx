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
        ifConsumed = success(input.position, _, _, toValue),
        ifRejected = message => failure(InvalidInput(input, message))
      )
    }
}

object CharacterParser {

  def string[A](value: InvariantView[Char] with HasPreciseSize, toValue: String => A): CharacterParser[A] = {
    val size = value.size
    CharacterParser(
      consume = { input =>
        if (input startsWith value) Consumed(value, input drop size)
        else Rejected(s"Expected `${value.force}`, got `${input.take(size).underlying.force[String]}`")
      },
      toValue = toValue compose (_.force)
    )
  }
}
