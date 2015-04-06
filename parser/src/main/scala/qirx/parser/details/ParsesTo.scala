package qirx.parser
package details

import psp.api.View
import shapeless.::
import shapeless.HNil
import shapeless.HList

trait ParsesTo[-A, B] {
  def apply(a: A, input: Input): Failure | View[Result[B]]
}
object ParsesTo {

  implicit def hnil =
    new (HNil ParsesTo HNil) {
      def apply(a: HNil, input: Input) = success(a, input)
    }

  implicit def hlist[A, T <: HList, B <: HList](
    implicit parseTail: T ParsesTo B
  ) =
    new ((Parser[A] :: T) ParsesTo (A :: B)) {

      def apply(a: Parser[A] :: T, input: Input) = {
        val parsedHead = a.head.parse(input)

        parsedHead.flatMapResult {
          case Result(value, remaining) => parseTail(a.tail, remaining) mapValue (value :: _)
        }
      }
    }
}
