package qirx.parser
package parsers

import psp.api._
import psp.std.{ Failure => _, _ }

case class NotParser[A, __](
  underlying : Parser[__],
  toValue    : InvariantView[Char] with HasPreciseSize => A
) extends Parser[A] {

  def parse(input: Input): Failure | View[Result[A]] = {
    if (input.isEmpty) Left(ExpectedInput(input))
    else {

      // We are using a manual fold left here
      var remaining = input

      val consumed =
        input.takeWhile { _ =>
          remaining.nonEmpty &&
            (underlying parse remaining).fold(
              ifLeft = { _ =>
                remaining = remaining.tail
                true
              },
              ifRight = _ => false
            )
        }

      success(toValue(consumed.underlying), remaining)
    }
  }
}
