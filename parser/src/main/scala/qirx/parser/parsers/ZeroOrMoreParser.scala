package qirx.parser
package parsers

import psp.api._
import psp.std.{Failure => _, _}

case class ZeroOrMoreParser[A, B](
  underlying : Parser[A],
  toValue    : View[A] => B
) extends Parser[B] {

  def parse(input: Input): Failure | View[Result[B]] = {

    var lastResult = ParseResult.withSingleResult(emptyValue[View[A]], input)
    var continue = true

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
