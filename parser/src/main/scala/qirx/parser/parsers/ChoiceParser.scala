package qirx.parser
package parsers

import psp.api._
import psp.std.{Failure => _, _}

case class ChoiceParser[A, B](
  parsers: View[Parser[A]],
  toValue: A => B
) extends Parser[B] {

  if (parsers.isEmpty) abort("Can not operate without any parsers.")

  def parse(input: InvariantView[Char]): Failure | View[Result[B]] = {

    val start = emptyValue[(View[Failure], View[Result[A]])]

    val (failures, successes) = parsers.foldl(start) {
      case ((failures, successes), parser) =>
        parser parse input match {
          case Left(failure)  => (failures :+ failure, successes)
          case Right(success) => (failures, successes ++ success)
        }
    }

    if (successes.isEmpty) Left(failures.head)
    else Right(successes.map(_.map(toValue)))
  }
}
