package qirx.language

import qirx.parser.grammar.Grammar
import psp.api._
import psp.std._
import qirx.parser.grammar.details.Constructor
import qirx.parser._
import qirx.parser.grammar.details.Translate
import qirx.parser.grammar.details.TransformedTo
import shapeless.HNil
import shapeless.::
import qirx.parser._
import qirx.parser.Failure
import qirx.parser.grammar._
import qirx.parser.parsers.CharacterParser
import qirx.parser.grammar.Custom
import qirx.parser.details.Consumed
import qirx.parser.details.Rejected
import qirx.parser.grammar.details.AsParserOf
import qirx.parser.details.Outcome
import qirx.language.parsers.NotOrEscapedParser
import qirx.language.details.CodePoint

object grammar extends Grammar
  with Parsers
  with Translations
  with Constructors
  with IdGrammar {

  Underscore  := `__`

  Literal     := Numeric | String | Char | Boolean | Null
    Null      := `null`
    String    := `"` ~ (!`"`).orEscaped ~ `"` // "
    Char      := `'` ~ (!`'`).orEscaped ~ `'` // '
    Boolean   := `true` | `false`
    Numeric   := Hex | Decimal
      Hex     := `0x` ~ HexDigits
      Decimal := Digits ~ (`.` ~ Digits).?
}
