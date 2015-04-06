package qirx.parser.grammar
package details

import psp.api.View
import psp.std._
import shapeless.::
import shapeless.HList
import shapeless.HNil
import shapeless.Generic

trait Constructor[A, B] extends (A => B) {
  def apply(a:A):B
}

object Constructor {

  implicit def conforms[A, B](implicit ev: A => B): Constructor[A, B] =
    new Constructor[A, B] {
      def apply(a:A) = ev(a)
    }

  implicit def generic[A, C <: HList, B](
    implicit gen: Generic.Aux[B, C], convert: A ToHList C): Constructor[A, B] =
      new Constructor[A, B] {
        def apply(a:A) = gen.from(convert(a))
      }

  abstract class ToHList[-A, B <: HList] {
    def apply(a:A):B
  }

  object ToHList {

    implicit def any[A] =
      new (A ToHList (A :: HNil)) {
        def apply(a: A) = a :: HNil
      }

    implicit def hnil =
      new (HNil ToHList HNil) {
        def apply(a:HNil) = a
      }

    implicit def sameHead[H, T1 <: HList, T2 <: HList](
      implicit tail: T1 ToHList T2
    ) =
      new ((H :: T1) ToHList (H :: T2)) {
        def apply(a: H :: T1) = a.head :: tail(a.tail)
      }

    implicit def viewPattern[H, T1 <: HList, T2 <: HList](
      implicit tail: ToHList[T1, T2]
    ) =
      new ((H :: View[H] :: T1) ToHList (View[H] :: T2)) {
        def apply(a: H :: View[H] :: T1) = (a.head +: a.tail.head) :: tail(a.tail.tail)
      }
  }
}
