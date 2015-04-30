package qirx.language

import psp.api._
import psp.std._
import qirx.parser.Result
import qirx.parser.grammar.details.Constructor
import shapeless.HList
import shapeless.HNil
import shapeless.::

trait Constructors {
  implicit def viewStringAsString(
    implicit asString: Constructor[String, String]
  ) =
    new Constructor[View[Result[String]], String] {
      def apply(s: Result[View[Result[String]]]) =
        s.map(_ map asString mkString "") |> asString
    }

  implicit def hnilString =
    new Constructor[HNil, String] {
      def apply(s: Result[HNil]) = ""
    }

  implicit def hlistStrings[H, T <: HList, A](
    implicit headAsString: Constructor[H, String],
             tailAsString: Constructor[T, String],
             construct: Constructor[String, A]
  ) =
    new Constructor[Result[H] :: T, A] {
      def apply(s: Result[Result[H] :: T]) = {
        val head = s.value.head |> headAsString
        val tail = s.map(_.tail) |> tailAsString
        s.map(_ => head + tail) |> construct
      }
    }

}
