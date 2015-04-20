package qirx.parser
package grammar

import psp.api._
import psp.std._
import qirx.parser.grammar.details.AsParserOf
import qirx.parser.grammar.details.Constructor
import qirx.parser.grammar.details.Translate
import qirx.parser.grammar.details.TransformedTo

trait Grammar {

  def variableCharacters: ExMap[Variable, ExSet[Char]]
  def fixedStrings: ExMap[Fixed, String]

  // This mutable construction was chosen to allow more freedom when defining productions. If
  // we were to make this an abstract class and passed in the methods through the constructor
  // you would be limited to the stuff that can be put into argument lists. On top of that, it
  // would require typing extra comma's

  private implicit def productionEq = HashEq.natural[Production[_]]
  private var _productions = emptyValue[ExSet[Production[_]]]

  def parserFor[T](nonterminal: Nonterminal[T]):Parser[T] =
    _productions
      .find(_.nonterminal == nonterminal)
      // Production binds the types of terminal and parser, this allows us to do a safe cast
      .map(_.parser.asInstanceOf[Parser[T]])
      .getOrElse(abort(s"You have not defined a parser for $nonterminal"))

  protected implicit class NonterminalOperations[N, R](nonterminal: N) {
    def := [E <: Element, F <: Element, T](element: E)(
      implicit asNonterminal : N => Nonterminal[R],
               customize     : (E TransformedTo F)#InContext[N],
               asParser      : F AsParserOf T,
               constructor   : Constructor[T, R]
    ): Unit = {

      object ConstructingParser extends Parser[R] {
        lazy val underlying = element |> customize |> asParser

        def parse(input: Input) =
          underlying parse input mapResult {
            result => result map (_ => constructor(result))
          }
      }

      val production = Production[R](nonterminal, ConstructingParser)
      // https://github.com/paulp/psp-std/issues/37
      _productions = (_productions :+ production).toExSet
    }
  }

  private case class Production[T](val nonterminal:Nonterminal[T], parser: Parser[T])

  // The following implicits are required to make the construction of parsers possible
  protected implicit lazy val freeTranslation: Translate[Variable, ExSet[Char]] = new Translate(
    variable => variableCharacters.get(variable) getOrElse abort("Could not find characters for " + variable)
  )
  protected implicit lazy val nonFreeTranslation: Translate[Fixed, String] = new Translate(
    fixed => fixedStrings.get(fixed) getOrElse abort("Could not find string for " + fixed)
  )
  protected implicit def nonterminalTranslation[T]:Translate[Nonterminal[T], Parser[T]] = new Translate(
    nonterminal => parserFor(nonterminal)
  )
}
