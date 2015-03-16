package qirx.parser
package parsers

import psp.api._
import psp.std.{ Failure => _, _ }

case class NotParser[A, __](
  underlying: Parser[__],
  toValue: InvariantView[Char] => A) extends Parser[A] {

  def parse(input: InvariantView[Char]): Failure | View[Result[A]] = {
    if (input.isEmpty) Left(ExpectedInput)
    else {

      // I have no clue if this can be solved differently using a more functional
      // style
      var remaining = input

      val consumed =
        input.takeWhile { _ =>
          remaining.nonEmpty &&
            (underlying parse remaining).fold(
              ifLeft = { _ =>
                remaining = remaining.tail.force
                true
              },
              ifRight = _ => false
            )
        }

      success(toValue(consumed.force), remaining)
    }
  }
}
