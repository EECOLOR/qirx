package qirx.parser.grammar.details

import shapeless.::
import shapeless.HList

trait TransformedTo[-I <: HList, +O <: HList] extends (I => O) {
  def apply(i: I):O
}

trait DefaultTransformedTo {
  implicit def identity[I <: HList] =
    new (I TransformedTo I) {
      def apply(i: I):I = i
    }
}

object TransformedTo extends DefaultTransformedTo

