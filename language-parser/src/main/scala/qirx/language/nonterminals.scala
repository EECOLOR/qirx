package qirx.language

import qirx.parser.grammar.Nonterminal

case object Numeric extends Nonterminal[ast.Numeric]
case object Decimal extends Nonterminal[ast.Decimal]
case object Hex extends Nonterminal[ast.Hex]

case object String extends Nonterminal[ast.StringValue]
