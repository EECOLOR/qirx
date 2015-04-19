package qirx.language

import qirx.parser.grammar.Grammar
import psp.api._
import psp.std._
import qirx.parser.grammar.Free
import psp.std.HashEq.universalEq
import qirx.parser.grammar.NonFree
import qirx.parser.grammar.details.Constructor
import qirx.parser._

object grammar extends Grammar {

  val freeCharacters: ExMap[Free, ExSet[Char]] = Direct(
    Digits    -> ExSet('0' to '9'),
    HexDigits -> ExSet(('a' to 'f') ++ ( 'A' to 'F') ++ ('0' to '9'))
  ).toExMap

  val nonFreeStrings: ExMap[NonFree, String] = Direct(
    `.`  -> ".",
    `0x` -> "0x",
    `"`  -> "\"",
    ` \" ` -> "\\\""
  ).toExMap

  Numeric := Hex | Decimal
  Decimal := Digits ~ (`.` ~ Digits).?
  Hex     := `0x` ~ HexDigits
  String  := `"` ~ (` \" ` | !`"`).* ~ `"`
}
