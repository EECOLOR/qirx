package qirx.language.parsers

import psp.api._
import psp.std._
import qirx.parser.Input
import qirx.parser.parsers.CharacterParser
import qirx.parser.details.Consumed
import qirx.parser.details.Rejected
import qirx.parser.details.Outcome

object SingleCharParser {

  def apply(chars: ToBool[Char], charsAsString: String) =
    CharacterParser(consume(chars, charsAsString), _.head)

  private def consume(chars: ToBool[Char], charsAsString: String)(input: Input): Outcome = {
    val firstChar = input.head
    if (chars(firstChar)) Consumed(Direct(firstChar), input.tail)
    else Rejected(s"Expected one of $charsAsString, got $firstChar")
  }
}
