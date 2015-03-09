package qirx.parser
package translation

import qirx.parser.grammar.Terminal
import qirx.parser.raw.RawAst
import scala.language.higherKinds

object DefaultTerminalTranslator extends TerminalTranslator {
  import qirx.parser.grammar.Terminal._

  def using[T[+_]](adapt: ParserAdapter[T]): Terminal => T[RawAst] = {
    val noCapture = adapt.char(_ => raw.NoCapture) _

    {
      case Id  => adapt.predicate(raw.Id)(('a' to 'z').toSet) // Once we have more tests we can expand this set

      // We should probably use some sort of map for this
      case `"` => noCapture('"')
      case `(` => noCapture('(')
      case `)` => noCapture(')')
    }
  }
}
