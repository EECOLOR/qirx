package qirx.parser
package parsers

import psp.api.View
import psp.std.conforms
import psp.std.emptyBaseView
import psp.std.emptyValue

case class ZeroOrMoreParser[A, B](
  underlying : Parser[A],
  toValue    : View[Result[A]] => B
) extends Parser[B] {

  def parse(input: Input): Failure | View[Result[B]] = {

    val firstResult = emptySuccess(emptyValue[View[Result[A]]], input)

    firstResult repeatWith underlying mapValue toValue
  }
}
