package qirx.parser.grammar.details

import shapeless.::
import shapeless.HNil
import shapeless.HList
import qirx.parser.Parser
import qirx.parser.grammar.Element

trait AsParserList[-E <: HList, P <: HList] extends (E => P) {
  def apply(list:E): P
}

object AsParserList {

  implicit def hnil =
    new (HNil AsParserList HNil) {
      def apply(list: HNil) = list
    }

  implicit def hlist[H <: Element, T <: HList, A, P <: HList](
    implicit asHeadParser  : H AsParserOf A,
             asTailParsers : T AsParserList P
  ) =
    new ((H :: T) AsParserList (Parser[A] :: P)) {
      def apply(list: H :: T) = asHeadParser(list.head) :: asTailParsers(list.tail)
    }
}
