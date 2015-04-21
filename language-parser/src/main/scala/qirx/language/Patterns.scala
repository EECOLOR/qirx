package qirx.language

import qirx.parser.grammar.details.TransformedTo
import qirx.parser.grammar.Not
import shapeless.HNil
import shapeless.::

trait Patterns {
  implicit def escapePattern[T <: GroupMarker, X] =
    TransformedTo.forContext[X] { (pattern: T :: Not[T] :: T :: HNil) =>
      val t = pattern.head
      t :: NotOrEscaped(t) :: t :: HNil
    }
}
