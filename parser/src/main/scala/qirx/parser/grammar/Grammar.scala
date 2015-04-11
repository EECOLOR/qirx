package qirx.parser
package grammar

import psp.api._
import psp.std._
import qirx.parser.details.ParsesTo
import qirx.parser.grammar.details.AsParserOf
import qirx.parser.grammar.details.Constructor
import qirx.parser.grammar.details.Translate

trait Grammar {

  def freeCharacters: ExMap[Free, ExSet[Char]]
  def nonFreeStrings: ExMap[NonFree, String]

  // This mutable construction was chosen to allow more freedom when defining productions. If
  // we were to make this an abstract class and passed in the methods through the constructor
  // you would be limited to the stuff that can be put into argument lists. On top of that, it
  // would require typing extra comma's

  private implicit def productionEq = HashEq.natural[Production[_]]
  private var _productions = emptyValue[ExSet[Production[_]]]

  def parserFor[T](nonterminal: Nonterminal[T]):Option[Parser[T]] =
    _productions
      .find(_.nonterminal == nonterminal)
      // Production binds the types of terminal and parser, this allows us to do a safe cast
      .map(_.parser.asInstanceOf[Parser[T]])

  protected implicit lazy val freeTranslation: Translate[Free, ExSet[Char]] = new Translate(
    free => freeCharacters.get(free) getOrElse abort("Could not find characters for " + free)
  )
  protected implicit lazy val nonFreeTranslation: Translate[NonFree, String] = new Translate(
    nonFree => nonFreeStrings.get(nonFree) getOrElse abort("Could not find string for " + nonFree)
  )
  protected implicit def nonterminalTranslation[T]:Translate[Nonterminal[T], Parser[T]] = new Translate(
    nonterminal => parserFor(nonterminal) getOrElse abort("Could not find parser for " + nonterminal)
  )

  protected implicit class NonterminalOperations[R](nonterminal: Nonterminal[R]) {
    def := [E <: Element, T](element: E)(
      implicit asParser: E AsParserOf T,
               constructor: Constructor[T, R]
    ):Unit = {
      val constructingParser =
        new Parser[R] {
          lazy val parser = asParser(element)
          def parse(input: Input) = parser parse input mapValue constructor
        }
      val production = new Production[R](nonterminal, constructingParser)
      // https://github.com/paulp/psp-std/issues/37
      _productions = (_productions :+ production).toExSet
    }
  }

  private class Production[+T](val nonterminal:Nonterminal[T], _parser: => Parser[T]) {
    lazy val parser = _parser
  }
}
