package qirx.parser
package parsers

import psp.api.View
import shapeless.::
import shapeless.HList
import qirx.parser.details.ParsesTo

case class SequenceParser[H, T <: HList, A <: HList, B](
  parsers: H :: T,
  toValue: A => B)(implicit parse: (H :: T) ParsesTo A) extends Parser[B] {

  def parse(input: Input): Failure | View[Result[B]] = {

    val result = parse(parsers, input)

    result mapValue toValue
  }
}
