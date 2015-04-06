package qirx.parser
package parsers

import psp.api.View
import psp.std.conforms
import psp.std.emptyBaseView
import psp.std.emptyValue

case class ZeroOrMoreParser[A, B](
  underlying : Parser[A],
  toValue    : View[A] => B
) extends Parser[B] {

  def parse(input: Input): Failure | View[Result[B]] = {

    val firstResult = success(emptyValue[View[A]], input)

    firstResult repeatWith underlying mapValue toValue
  }
}
