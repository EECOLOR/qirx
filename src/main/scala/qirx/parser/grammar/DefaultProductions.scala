package qirx.parser.grammar

object DefaultProductions extends Productions {
  import Nonterminal._, Terminal._
  import Element.Operations

  TopLevelStatements := Id ~ `(` ~ String ~ `)`

  String := `"` ~ !`"` ~ `"` // "
}
