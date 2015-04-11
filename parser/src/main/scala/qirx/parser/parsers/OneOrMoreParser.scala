package qirx.parser
package parsers

import psp.api.View
import psp.std.conforms

case class OneOrMoreParser[A, B](
  underlying : Parser[A],
  toValue    : View[Result[A]] => B
) extends Parser[B] {

  def parse(input: Input): Failure | View[Result[B]] = {

    def toResultView: Result[A] => Result[View[Result[A]]] =
      result => result map (_ => newView(result))

    val firstResult = underlying parse input mapResult toResultView

    firstResult repeatWith underlying mapValue toValue
  }
}
