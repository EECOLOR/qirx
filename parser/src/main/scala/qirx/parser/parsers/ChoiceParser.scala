package qirx.parser
package parsers

import psp.api.View
import psp.api.IsEmpty
import psp.std.abort
import psp.std.upcastForView
import psp.std.viewToEach

case class ChoiceParser[A, B](
  parsers: View[Parser[A]],
  toValue: A => B
) extends Parser[B] {

  if (parsers.isEmpty) abort("Can not operate without any parsers.")

  def parse(input: Input): Failure | View[Result[B]] = {

    val result = parsers.map(_ parse input).flatMap(_.toResultView).toParseResult

    result mapValue toValue
  }
}
