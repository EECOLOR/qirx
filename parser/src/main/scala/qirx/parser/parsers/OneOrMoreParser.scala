package qirx.parser
package parsers

import psp.api.View
import psp.std.conforms

case class OneOrMoreParser[A, B](
  underlying : Parser[A],
  toValue    : View[A] => B
) extends Parser[B] {

  def parse(input: Input): Failure | View[Result[B]] = {

    def toView(x: A): View[A] = newView(x)
    val firstResult = underlying parse input mapValue toView

    firstResult repeatWith underlying mapValue toValue
  }
}
