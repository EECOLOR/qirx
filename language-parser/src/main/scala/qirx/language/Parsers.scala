package qirx.language

import psp.std.String
import qirx.language.parsers.NotOrEscapedParser
import qirx.parser.grammar.details.Translate
import qirx.parser.grammar.details.AsParserOf
import qirx.parser.grammar.Fixed

trait Parsers {
  implicit def notOrEscaped[T <: GroupMarker](
    implicit fixedStrings: Translate[Fixed, String]
  ) =
    new (NotOrEscaped[T] AsParserOf String) {
      def apply(notOrEscaped: NotOrEscaped[T]) =
        NotOrEscapedParser(fixedStrings(notOrEscaped.value), fixedStrings(`\\`))
    }
}
