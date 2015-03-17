package qirx.parser
package parsers

import psp.api._
import psp.std.{ Failure => _, _ }

case class SequenceParser[A, B](
  parsers: View[Parser[A]],
  toValue: View[A] => B
) extends Parser[B] {

  if (parsers.isEmpty) abort("Can not operate without any parsers.")

  def parse(input: InvariantView[Char]): Failure | View[Result[B]] = {

    val start = ParseResult.withSingleResult(emptyValue[View[A]], input)

    val result = parsers.foldl(start)(ParseResult.parseAndConcatenate)

    result transform toValue
  }
}
