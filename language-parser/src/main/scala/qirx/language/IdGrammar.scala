package qirx.language

import psp.api._
import psp.std._
import qirx.parser.grammar.Grammar
import qirx.parser.grammar.Element
import qirx.parser.grammar.Nonterminal
import qirx.parser.grammar.details.AsParserOf

/*
 * This part of the grammar is not included in the main grammar because it
 * contains too much details.
 */
trait IdGrammar { _: Grammar with Constructors with Parsers =>

  import Id._

  Id := RegularId | EscapedId
def as[A <: Element, B](a:A)(implicit ev: A AsParserOf B):B = null.asInstanceOf[B]

  val x = as((` _ ` | `$`).+ ~ (AlphaNum.+ | OperatorChar.+))

  case object RegularId extends Nonterminal[ast.RegularId]
  case object EscapedId extends Nonterminal[ast.EscapedId]
  case object End extends Nonterminal[String]
  case object OperatorStart extends Nonterminal[String]

  RegularId     := (OperatorStart | LetterStart | SpecialStart) ~ Rest.* ~ ` _ `.*
  EscapedId     := `‘` ~ (!`‘`).orEscaped ~ `‘`
  LetterStart   := Letter ~ AlphaNum.*
  OperatorStart := OperatorChar.+
  SpecialStart  := (` _ ` | `$`).+ ~ (AlphaNum.+ | OperatorChar.+)
  Rest          := ` _ `.+ ~ (AlphaNum.+ | OperatorChar.+)
  AlphaNum      := Letter | Digit
}
