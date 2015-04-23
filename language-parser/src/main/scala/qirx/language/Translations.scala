package qirx.language

import psp.api._
import psp.std._
import psp.std.HashEq.universalEq
import qirx.parser.grammar.Fixed
import qirx.parser.grammar.Variable

trait Translations {
  val variableCharacters: ExMap[Variable, ExSet[Char]] = Direct(
    Digits    -> ExSet('0' to '9'),
    HexDigits -> ExSet(('a' to 'f') ++ ( 'A' to 'F') ++ ('0' to '9'))
  ).toExMap

  val fixedStrings: ExMap[Fixed, String] = Direct(
    `.`  -> ".",
    `0x` -> "0x",
    `"`  -> "\"",
    `\\` -> "\\",
    `'`  -> "'",
    `__`  -> "_",

    `true`  -> "true",
    `false` -> "false",
    `null`  -> "null"
  ).toExMap
}
