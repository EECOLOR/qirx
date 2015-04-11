package qirx.parser
package parsers

import psp.api.View
import psp.std.Option
import psp.std.None
import qirx.parser.details.Succeeded

case class ZeroOrOneParser[A, B](
  underlying : Parser[A],
  toValue    : Option[A] => B
) extends Parser[B] {

  def parse(input: Input): Failure | View[Result[B]] = {
    val result =
      (underlying parse input).fold(
        ifFailure = _       => newView(emptyResult(toValue(None), input)),
        ifSuccess = results => results.map(_.map(toValue compose Option.apply))
      )
    Succeeded(result)
  }
}
