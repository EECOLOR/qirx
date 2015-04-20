package qirx.parser
package parsers

import psp.api.InvariantView
import psp.api.HasPreciseSize
import psp.api.View
import psp.std.Char

case class NotParser[A](
  underlying : Parser[_],
  toValue    : InvariantView[Char] with HasPreciseSize => A
) extends Parser[A] {

  def parse(input: Input): Failure | View[Result[A]] = {
    if (input.isEmpty) failure(ExpectedInput(input))
    else {

      // We are using a manual fold left here
      var remaining = input

      val consumed =
        input.takeWhile { _ =>
          remaining.nonEmpty &&
            (underlying parse remaining).fold(
              ifFailure = { _ =>
                remaining = remaining.tail
                true
              },
              ifSuccess = _ => false
            )
        }

      success(input.position, consumed.underlying, remaining, toValue)
    }
  }
}
