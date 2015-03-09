package qirx.parser.grammar

abstract class Productions {

  private[this] var _productions = Set.empty[Production]
  def productions: Set[Production] = _productions

  implicit val addProduction:Productions.AddProduction =
    new Productions.AddProduction {
      def apply(nonterminal:Nonterminal, element: Element): Unit =
        _productions += nonterminal -> element
    }
}

object Productions {
  abstract class AddProduction extends ((Nonterminal, Element) => Unit)

  implicit val defaults = DefaultProductions
}
