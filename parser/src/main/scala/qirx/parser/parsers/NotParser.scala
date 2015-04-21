package qirx.parser
package parsers

import psp.api._
import psp.std._
import qirx.parser.Failure

case class NotParser[A](
  underlying : Parser[_],
  toValue    : Char => A
) extends Parser[A] {

  def parse(input: Input): Failure | View[Result[A]] = {
    if (input.isEmpty) failure(ExpectedInput(input))
    else {

      val firstChar = input.head

      (underlying parse input).fold(
        ifSuccess = _ => failure(InvalidInput(input, s"Expected underlying parser to reject `$firstChar`, it consumed it")),
        ifFailure = _ => success(input.position, input.tail, toValue(firstChar))
      )
    }
  }
}
