package qirx.language.parsers

import psp.api._
import psp.std._
import qirx.language.GroupMarker
import qirx.language.`\\`
import qirx.parser.details.Consumed
import qirx.parser.details.Outcome
import qirx.parser.Input
import qirx.parser.Parser
import qirx.parser.parsers.CharacterParser

object NotOrEscapedParser {

  def apply(value: String, escape: String): Parser[String] =
    CharacterParser(consume(value, escape), _.force)

  private def consume(value: String, escape: String): Input => Outcome = {
    val escapedValue = escape + value
    val escapedValueSize = escapedValue.size

    { input =>
      var outcome = Consumed(emptyValue[View[Char]].force, input)
      var continue = true

      while (continue && outcome.remaining.nonEmpty) {
        val Consumed(consumed, remaining) = outcome
        if (remaining startsWith escapedValue)
          outcome = Consumed((consumed ++ value).force, remaining drop escapedValueSize)
        else if (remaining startsWith value)
          continue = false
        else
          outcome = Consumed((consumed :+ remaining.head).force, remaining.tail)
      }

      outcome
    }
  }
}
