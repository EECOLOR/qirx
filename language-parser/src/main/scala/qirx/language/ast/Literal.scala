package qirx.language
package ast

import psp.std.Option
import psp.std.String
import qirx.parser.grammar.Positioned

sealed trait Literal extends Positioned

// This is not an object because we want to be able to record it's position
case class NullValue() extends Literal

case class CharValue(value: String) extends Literal
case class StringValue(value: String) extends Literal
case class BooleanValue(value: BooleanFeature) extends Literal

sealed trait Numeric extends Literal
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
