package qirx.parser
package parsers

import psp.api.View
import SequenceParser.ParsesTo
import shapeless.HList
import shapeless.HNil
import shapeless.::

case class SequenceParser[H, T <: HList, A <: HList, B](
  parsers: H :: T,
  toValue: A => B
)(implicit parse: (H :: T) ParsesTo A) extends Parser[B] {

  def parse(input: Input): Failure | View[Result[B]] = {

    val result = parse(parsers, input)

    result mapValue toValue
  }
}

object SequenceParser {

  trait ParsesTo[-A, B] {
    def apply(a: A, input: Input): Failure | View[Result[B]]
  }

  object ParsesTo {

    implicit def hnil =
      new (HNil ParsesTo HNil) {
        def apply(a: HNil, input: Input) = emptySuccess(a, input)
      }

    implicit def hlist[A, T <: HList, B <: HList](
      implicit parseTail: T ParsesTo B
    ) =
      new ((Parser[A] :: T) ParsesTo (Result[A] :: B)) {

        def apply(a: Parser[A] :: T, input: Input) = {
          val parsedHead = a.head.parse(input)

          parsedHead.flatMapResult {
            case result @ Result(_, position, remaining) =>
              val parsedTail = parseTail(a.tail, remaining)
              parsedTail.mapResult {
                case Result(newValue, newPosition, newRemaining) =>
                  Result(result :: newValue, Position(position.start, newPosition.end), newRemaining)
              }
          }
        }
      }
  }
}
