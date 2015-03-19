package qirx.grammar

import psp.api._
import psp.std._
import psp.std.HashEq.universalEq

trait Productions {

  // This mutable construction was chosen to allow more freedom when defining productions. If
  // we were to make this an abstract class and passed in the methods through the constructor
  // you would be limited to the stuff that can be put into argument lists.

  private[this] var _productions = emptyValue[ExMap[Nonterminal, Element]]

  def productions: ExMap[Nonterminal, Element] = _productions

  protected[this] implicit class NonterminalOperations(nonterminal: Nonterminal) {
    def := (element: Element):Unit = _productions += (nonterminal, element)
  }
}
