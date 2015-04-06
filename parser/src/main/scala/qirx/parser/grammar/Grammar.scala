package qirx.parser
package grammar

import psp.api._
import psp.std._
import qirx.parser.details.ParsesTo
import qirx.parser.grammar.details.AsParserOf
import qirx.parser.grammar.details.Translate

trait Grammar { _: FreeCharacters with NonFreeStrings =>

  protected implicit lazy val freeTranslation: Translate[Free, ExSet[Char]] = new Translate(
    free => freeCharacters.get(free) getOrElse abort("Could not find characters for " + free)
  )
  protected implicit lazy val nonFreeTranslation: Translate[NonFree, String] = new Translate(
    nonFree => nonFreeStrings.get(nonFree) getOrElse abort("Could not find string for " + nonFree)
  )
  protected implicit def nonterminalTranslation[T]:Translate[Nonterminal[T], Parser[T]] = new Translate(
    nonterminal => parser(nonterminal) getOrElse abort("Could not find parser for " + nonterminal)
  )

  // This mutable construction was chosen to allow more freedom when defining productions. If
  // we were to make this an abstract class and passed in the methods through the constructor
  // you would be limited to the stuff that can be put into argument lists. On top of that, it
  // would require typing extra comma's

  private implicit def productionEq = HashEq.natural[Production[_]]
  private var _productions = emptyValue[ExSet[Production[_]]]

  def parser[T](nonterminal: Nonterminal[T]):Option[Parser[T]] =
    _productions
      .find(_.nonterminal == nonterminal)
      // Production binds the types of terminal and parser, this allows us to do a safe cast
      .map(_.parser.asInstanceOf[Parser[T]])

  protected implicit class NonterminalOperations[R](nonterminal: Nonterminal[R]) {
    def := [E <: Element](element: E)(implicit asParser: E AsParserOf R):Unit = {
      val production = new Production(nonterminal, asParser(element))
      // https://github.com/paulp/psp-std/issues/37
      _productions = (_productions :+ production).toExSet
    }
  }

  private class Production[+T](val nonterminal:Nonterminal[T], _parser: => Parser[T]) {
    lazy val parser = _parser
  }
}
