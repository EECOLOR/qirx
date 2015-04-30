package qirx.language

import psp.std.Char
import psp.std.String
import qirx.language.parsers.NotOrEscapedParser
import qirx.parser.grammar.details.Translate
import qirx.parser.grammar.details.AsParserOf
import qirx.parser.grammar.Fixed
import qirx.parser.grammar.Not
import qirx.language.parsers.SingleCharParser

trait Parsers {

  implicit def notOrEscaped[T <: GroupMarker](
    implicit fixedStrings: Translate[Fixed, String]
  ) =
    new (NotOrEscaped[T] AsParserOf String) {
      def apply(notOrEscaped: NotOrEscaped[T]) =
        NotOrEscapedParser(fixedStrings(notOrEscaped.value), fixedStrings(`\\`))
    }

  implicit class NotOrEscapedForElement[T <: GroupMarker](e: Not[T]) {
    def orEscaped = NotOrEscaped(e.element)
  }

  implicit def singleCharParser =
    new (SingleChar AsParserOf Char) {
      def apply(singleChar: SingleChar) =
        SingleCharParser(singleChar.chars, singleChar.charsAsString)
    }
}
