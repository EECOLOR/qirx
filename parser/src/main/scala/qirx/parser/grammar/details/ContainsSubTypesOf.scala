package qirx.parser.grammar
package details

import psp.api.<:<
import shapeless.HList
import shapeless.HNil
import shapeless.::

/**
 * Type witness that requires each element of the HList to extend a certain type.
 */
trait ContainsSubTypesOf[A <: HList, B]

object ContainsSubTypesOf {

  implicit def hnil[A]:HNil ContainsSubTypesOf A = null

  implicit def hlist[H, T <: HList, A](
    implicit headEv: H <:< A,
             tailEv: T ContainsSubTypesOf A
  ): (H :: T) ContainsSubTypesOf A = null
}
