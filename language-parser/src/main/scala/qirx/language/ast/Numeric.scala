package qirx.language.ast

import psp.std.Option
import psp.std.String
import qirx.parser.grammar.Positioned

sealed trait Numeric extends Positioned
case class Decimal(
  integerPart: String,
  fractionalPart: Option[String]
) extends Numeric {
  /* Could be long, float, double or integer, I think it's not necessary to add support
   * for L, l, F, f, D and d, the compiler should be able to figure out what it should be.
   *
   * If you find an example where it would be impossible to determine the correct
   * representation from the context, let me know.
   */
}
case class Hex(value: String) extends Numeric

