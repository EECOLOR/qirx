package qirx.parser.grammar
package details

import psp.api._
import shapeless.::
import shapeless.HNil
import shapeless.HList

trait ContainsSubTypesOf[A, B]

object ContainsSubTypesOf {
  implicit def hnil[A]:HNil ContainsSubTypesOf A = null
  implicit def hlist[H, T <: HList, A](
    implicit headEv: H <:< A,
             tailEv: T ContainsSubTypesOf A
  ): (H :: T) ContainsSubTypesOf A = null
}
