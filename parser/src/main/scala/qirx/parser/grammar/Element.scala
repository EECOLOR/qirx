package qirx.parser.grammar

import shapeless.HList
import qirx.parser.grammar.details.ContainsSubTypesOf
import qirx.parser.grammar.details.ElementOperations
import psp.std.NaturalHashEq

sealed trait Element extends NaturalHashEq

object Element extends ElementOperations

case class Sequence[+E <: HList](elements: E)(implicit ev: E ContainsSubTypesOf Element) extends Element

case class Choice[+E <: HList](options: E)(implicit ev: E ContainsSubTypesOf Element) extends Element

case class OneOrMore [E <: Element](element: E) extends Element
case class ZeroOrMore[E <: Element](element: E) extends Element
case class ZeroOrOne [E <: Element](element: E) extends Element

case class Not[E <: Element](element: E) extends Element

trait Nonterminal[+R] extends Element

sealed trait Terminal extends Element

trait Free extends Terminal

sealed trait NonFree extends Terminal

trait Feature     extends NonFree
trait Keyword     extends NonFree
trait Separator   extends NonFree
trait GroupMarker extends NonFree
