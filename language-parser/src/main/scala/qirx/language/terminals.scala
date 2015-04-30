package qirx.language

import psp.std._
import qirx.parser.grammar.Free
import qirx.parser.grammar.Keyword
import qirx.parser.grammar.Feature
import qirx.parser.grammar.details.Constructor
import qirx.parser.Result
import shapeless.HNil

trait Separator extends Keyword
trait GroupMarker extends Keyword

case object Digits extends Free
case object HexDigits extends Free

sealed trait BooleanFeature extends Feature
case object `true`  extends BooleanFeature
case object `false` extends BooleanFeature

sealed trait `null` extends Feature
case object `null` extends `null` {
  implicit def nullValue(
    implicit constructor: Constructor[HNil, ast.NullValue]
  ) =
    new Constructor[`null`, ast.NullValue] {
      def apply(result: Result[`null`]) = result.map(_ => HNil) |> constructor
    }
}

sealed trait `__` extends Feature
case object `__`  extends `__` {
  implicit def underscore(
    implicit constructor: Constructor[HNil, ast.Underscore]
  ) =
    new Constructor[`__`, ast.Underscore] {
      def apply(result: Result[`__`]) = result.map(_ => HNil) |> constructor
    }
}

// The current implementation of back ticks is not really what it needs to be
case object `.`  extends Separator
case object `"`  extends GroupMarker // "
case object `0x` extends Keyword
case object `\\` extends Keyword
case object `'`  extends GroupMarker // '
case object `‘`  extends GroupMarker // ‘
