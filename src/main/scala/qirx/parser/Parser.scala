package qirx.parser

import qirx.parser.grammar.DefaultProductions
import qirx.parser.raw.RawAst
import qirx.parser.scalaparsercombinators.ScalaParserCombinatorsParserAdapter
import qirx.parser.translation.DefaultTerminalTranslator

trait Parser {
  def parse(value:String):RawAst
}

object Parser {
  object Default extends DefaultParser(
    DefaultProductions.productions,
    ScalaParserCombinatorsParserAdapter.`I hate it when people force me to do this`,
    DefaultTerminalTranslator
  )
}
