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
import qirx.parser.details.Outcome
import qirx.language.parsers.NotOrEscapedParser

object grammar extends Grammar with Patterns with Parsers with Translations with Constructors {

  Literal     := Numeric | String | Char | Boolean | Null
    Null      := `null`
    String    := `"` ~ !`"` ~ `"` // "
    Char      := `'` ~ !`'` ~ `'` // '
    Boolean   := `true` | `false`
    Numeric   := Hex | Decimal
      Hex     := `0x` ~ HexDigits
      Decimal := Digits ~ (`.` ~ Digits).?
}
