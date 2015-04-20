package qirx.language

import qirx.parser.grammar.Grammar
import psp.api._
import psp.std._
import psp.std.StdEq._
import psp.std.HashEq.universalEq
import qirx.parser.grammar.details.Constructor
import qirx.parser._
import qirx.parser.grammar.details.Translate
import qirx.parser.grammar.details.TransformedTo
import shapeless.HNil
import shapeless.::
import qirx.parser.grammar._
import qirx.parser.parsers.CharacterParser
import qirx.parser.grammar.Custom
import qirx.parser.details.Consumed
import qirx.parser.grammar.details.AsParserOf

object grammar extends Grammar {

  val variableCharacters: ExMap[Variable, ExSet[Char]] = Direct(
    Digits    -> ExSet('0' to '9'),
    HexDigits -> ExSet(('a' to 'f') ++ ( 'A' to 'F') ++ ('0' to '9'))
  ).toExMap

  val fixedStrings: ExMap[Fixed, String] = Direct(
    `.`  -> ".",
    `0x` -> "0x",
    `"`  -> "\"",
    `\\`  -> "\\"
  ).toExMap

  trait `\\` extends Keyword
  object `\\` extends `\\`

  case class Escaped[T <: GroupMarker](t: T) extends Custom

  implicit def escaped[T <: GroupMarker](
    implicit fixedStrings: Translate[Fixed, String]
  ) =
    new (Escaped[T] AsParserOf String) {
      def apply(escaped: Escaped[T]) = {
        val value = fixedStrings(escaped.t)
        val escapedValue = fixedStrings(`\\`) + value
        val escapedValueSize = escapedValue.size
        CharacterParser(
          input => {
            var outcome = Consumed(emptyValue[View[Char]].force, input)
            var continue = true

            while (continue && outcome.remaining.nonEmpty) {
              val Consumed(consumed, remaining) = outcome
              if (remaining.startsWith(escapedValue))
                outcome = Consumed((consumed ++ value).force, remaining.drop(escapedValueSize))
              else if (remaining.startsWith(value))
                continue = false
              else
                outcome = Consumed((consumed :+ remaining.head).force, remaining.tail)
            }
            outcome
          },
          _.force
        )
      }
    }

  implicit def escapePattern[T <: GroupMarker, X] =
    TransformedTo.forContext[X] { (i: T :: Not[T] :: T :: HNil) =>
      val t = i.head
      t :: Escaped(t) :: t :: HNil
    }

  Numeric := Hex | Decimal
  Decimal := Digits ~ (`.` ~ Digits).?
  Hex     := `0x` ~ HexDigits
  String  := `"` ~ !`"` ~ `"`
}
