package qirx.grammar

import psp.api._
import psp.std._

sealed trait Element extends NaturalHashEq

object Element extends ElementOperations

case class Sequence(elements: Direct[Element]) extends Element
object Sequence {
  def apply(elements: Element *): Sequence = Sequence(Direct.empty ++ elements.toVector)
}

case class Choice(options: Direct[Element]) extends Element
object Choice {
  def apply(elements: Element *): Choice = Choice(Direct.empty ++ elements.toVector)
}

case class AttributedElement(element: Element)(
  val oneOrMore  : Boolean = false,
  val zeroOrMore : Boolean = false,
  val zeroOrOne  : Boolean = false,
  val not        : Boolean = false
) extends Element

trait Nonterminal extends Element
trait Terminal    extends Element
