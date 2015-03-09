package qirx.grammar

import psp.api._
import psp.std._
import psp.std.HashEq.universalEq
import psp.std.StdEq._
import psp.std.StdZero._

trait Productions {

  private[this] var _productions = emptyValue[ExMap[Nonterminal, Element]]

  def productions = _productions

  protected[this] implicit class NonterminalOperations(nonterminal: Nonterminal) {
    def := (element: Element):Unit =
      // https://github.com/paulp/psp-std/issues/37
      _productions = (_productions :+ nonterminal -> element).byEquals.toMap
  }
}
