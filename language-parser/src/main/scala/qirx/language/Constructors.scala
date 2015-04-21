package qirx.language

import psp.std._
import qirx.parser.grammar.details.Constructor
import qirx.parser.grammar.details.Constructor.utilities.AddPositionsTo
import qirx.parser.Result
import shapeless.HNil

trait Constructors {
  implicit def nullValue(
    implicit constructor: Constructor[HNil, ast.NullValue]
  ) =
    new Constructor[`null`, ast.NullValue] {
      def apply(result: Result[`null`]) = result.map(_ => HNil) |> constructor
    }
}
