package qirx.language

import psp.api._
import psp.std._
import qirx.parser.Result
import qirx.parser.grammar.Nonterminal
import qirx.parser.grammar.details.Constructor

case object Id extends Nonterminal[ast.Id] {
  case object ` _ ` extends SingleChar('_')
  case object `$` extends SingleChar('$')
  case object Letter extends SingleChar("letter", _.isLetter)
  case object OperatorChar extends SingleChar("operator", _.isOperator)
  case object Digit extends SingleChar('0' to '9')

  case object LetterStart extends Nonterminal[String]
  case object SpecialStart extends Nonterminal[String]
  case object Rest extends Nonterminal[String]
  case object AlphaNum extends Nonterminal[Char]
}

case object Underscore extends Nonterminal[ast.Underscore]

case object Literal extends Nonterminal[ast.Literal]

case object Null extends Nonterminal[ast.NullValue]

case object Numeric extends Nonterminal[ast.Numeric]
case object Decimal extends Nonterminal[ast.Decimal]
case object Hex extends Nonterminal[ast.Hex]

case object String extends Nonterminal[ast.StringValue]
case object Char   extends Nonterminal[ast.CharValue]

object Boolean extends Nonterminal[ast.BooleanValue]
