package qirx.language

import qirx.parser.grammar.Nonterminal


case object Literal extends Nonterminal[ast.Literal]

case object Null extends Nonterminal[ast.NullValue]

case object Numeric extends Nonterminal[ast.Numeric]
case object Decimal extends Nonterminal[ast.Decimal]
case object Hex extends Nonterminal[ast.Hex]

case object String extends Nonterminal[ast.StringValue]
case object Char   extends Nonterminal[ast.CharValue]

object Boolean extends Nonterminal[ast.BooleanValue]
