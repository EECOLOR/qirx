package qirx.parser
package parsers

import psp.api._
import psp.api.Api._
import psp.std.{Failure => _, _}

case class CharacterParser[A](
  consume: InvariantView[Char] => Split[Char] /* TODO: this should be SplitView, but that has no unapply */,
  toValue: InvariantView[Char] => A
) extends Parser[A] {

  def parse(input: InvariantView[Char]): Failure | View[Result[A]] =
    if (input.isEmpty) failure(ExpectedInput)
    else {
      val Split(consumed, remaining) = consume(input)
      if (consumed.isEmpty) failure(InvalidInput)
      else success(toValue(consumed.force), remaining)
    }
}
