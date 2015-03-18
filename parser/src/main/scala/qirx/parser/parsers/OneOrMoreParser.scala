package qirx.parser
package parsers

import psp.api._
import psp.std.{Failure => _, _}

case class OneOrMoreParser[A, B](
  underlying : Parser[A],
  toValue    : View[A] => B
) extends Parser[B] {

  def parse(input: Input): Failure | View[Result[B]] = {

    def toView(x:A):View[A] = newView(x)
    var lastResult = ParseResult(ParseResult(underlying parse input) transform toView)
    var continue = lastResult.isSuccess

    while(continue) {
      val parseResult = ParseResult.parseAndConcatenate(lastResult, underlying)
      continue = parseResult.isSuccess
      if (continue) {
        lastResult = parseResult
      }
    }

    lastResult transform toValue
  }
}
