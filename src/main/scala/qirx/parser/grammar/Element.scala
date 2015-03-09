package qirx.parser
package grammar

sealed trait Element

object Element {
  implicit class Operations(element:Element) {
    def ~ (tail:Element):Sequence = new Sequence(element, tail)

    def unary_! : AttributedElement = AttributedElement(element)(not = true)
  }
}

case class AttributedElement(element: Element)(
  val not : Boolean = false
) extends Element

case class Sequence(head: Element, tail : Element) extends Element

// we can lose this once we have union types
sealed trait `Nonterminal | Terminal` extends Element

sealed trait Terminal extends `Nonterminal | Terminal`

object Terminal {
  case object Id extends Terminal

  case object `"` extends Terminal
  case object `(` extends Terminal
  case object `)` extends Terminal
}

sealed trait Nonterminal extends `Nonterminal | Terminal` {
  def := (element:Element)(implicit addProduction: Productions.AddProduction):Unit =
    addProduction(this, element)
}

object Nonterminal {

  object TopLevelStatements extends Nonterminal

  object String extends Nonterminal

}

