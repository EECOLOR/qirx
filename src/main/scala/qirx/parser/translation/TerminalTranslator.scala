package qirx.parser.translation

import qirx.parser.grammar.Terminal
import qirx.parser.raw.RawAst
import scala.language.higherKinds

trait TerminalTranslator {
  def using[T[+_]](adapt: ParserAdapter[T]): Terminal => T[RawAst]
}
