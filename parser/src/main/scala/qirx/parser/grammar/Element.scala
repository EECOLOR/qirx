package qirx.parser.grammar

import psp.std.NaturalHashEq
import psp.std.String
import psp.std.Unit
import qirx.parser.grammar.details.ContainsSubTypesOf
import qirx.parser.grammar.details.ElementOperations
import shapeless.HList

sealed trait Element extends NaturalHashEq

object Element extends ElementOperations

trait Custom extends Element

case class Sequence[+E <: HList](elements: E)(implicit ev: E ContainsSubTypesOf Element) extends Element

case class Choice[+E <: HList](options: E)(implicit ev: E ContainsSubTypesOf Element) extends Element

case class OneOrMore [+E <: Element](element: E) extends Element
case class ZeroOrMore[+E <: Element](element: E) extends Element
case class ZeroOrOne [+E <: Element](element: E) extends Element

case class Not[+E <: Element](element: E) extends Element

trait Nonterminal[+R] extends Element

sealed trait Terminal extends Element

trait Fixed    extends Terminal
trait Variable extends Terminal

trait Free  extends Variable with Capture[String]
trait Scrap extends Variable with Capture[Unit]

trait Feature extends Fixed with Capture[Capture.Self]
trait Keyword extends Fixed with Capture[Unit]

trait Capture[T]
object Capture {
  // Marker trait to capture the class or object itself
  sealed trait Self
}
