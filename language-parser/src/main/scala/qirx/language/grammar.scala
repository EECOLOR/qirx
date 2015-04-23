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
import qirx.parser.details.Rejected
import qirx.parser.grammar.details.AsParserOf
import qirx.parser.details.Outcome
import qirx.language.parsers.NotOrEscapedParser
import qirx.language.details.CodePoint

object grammar extends Grammar with Patterns with Parsers with Translations with Constructors {

  class SingleChar private(val chars: ToBool[Char], val charsAsString: String) extends Custom {
    def this(char: Char) = this((_: Char) === char, char.toString)
    def this(chars: Char *) = this(chars.toDirect.to[ExSet], chars mkString ", ")
    def this(direct: Direct[Char]) = this(direct.to[ExSet], direct mkString ", ")
    def this(charsAsString: String, f: CodePoint => Boolean) = this((_: Char) |> CodePoint.apply |> f, charsAsString)
  }
  implicit def singleCharParser =
    new (SingleChar AsParserOf Char) {
      def apply(singleChar: SingleChar) = {
        val chars = singleChar.chars
        CharacterParser({
          input =>
            val firstChar = input.head
            if (chars(firstChar)) Consumed(Direct(firstChar), input.tail)
            else Rejected(s"Expected one of ${singleChar.charsAsString}, got $firstChar")
        },
        _.head
        )
      }
    }

  case object Id extends Nonterminal[ast.Id]

  case object ` _ ` extends SingleChar('_')
  case object `_ | €` extends SingleChar('_', '$')
  case object Letter extends SingleChar("letter", _.isLetter)
  case object OperatorChar extends SingleChar("operator", _.isOperator)
  case object Digit extends SingleChar('0' to '9')

  case object LetterStart extends Nonterminal[String]
  case object SpecialStart extends Nonterminal[String]
  case object Rest extends Nonterminal[String]
  case object AlphaNum extends Nonterminal[Char]

  AlphaNum := Letter | Digit
  LetterStart := Letter ~ AlphaNum.*
  SpecialStart := `_ | €` ~ (AlphaNum.+ | OperatorChar.+)
  Rest := ` _ ` ~ (AlphaNum.+ | OperatorChar.+)

  implicit def xConstructor[A](
    implicit asString: Constructor[String, String],
             constructor: Constructor[String, ast.Id]
  ) =
    new Constructor[View[Result[String]], ast.Id] {
      def apply(s: Result[View[Result[String]]]) = s.map(_ map asString mkString "") |> constructor
    }

  Id := (LetterStart | SpecialStart) ~ Rest.*

  Underscore  := `__`

  Literal     := Numeric | String | Char | Boolean | Null
    Null      := `null`
    String    := `"` ~ !`"` ~ `"` // "
    Char      := `'` ~ !`'` ~ `'` // '
    Boolean   := `true` | `false`
    Numeric   := Hex | Decimal
      Hex     := `0x` ~ HexDigits
      Decimal := Digits ~ (`.` ~ Digits).?
}
