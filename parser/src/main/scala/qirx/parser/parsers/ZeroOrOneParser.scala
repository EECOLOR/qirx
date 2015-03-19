package qirx.parser
package parsers

import psp.api._
import psp.std.{ Failure => _, _ }

case class ZeroOrOneParser[A, B](
  underlying : Parser[A],
  toValue    : Option[A] => B
) extends Parser[B] {

  def parse(input: Input): Failure | View[Result[B]] = {
    val result =
      (underlying parse input).fold(
        ifLeft  = _       => newView(Result(toValue(None), input)),
        ifRight = results => results.map(_.map(toValue compose Option.apply))
      )
    Right(result)
  }
}
