package qirx.parser

import qirx.parser.grammar.AttributedElement
import qirx.parser.grammar.Element
import qirx.parser.grammar.Nonterminal
import qirx.parser.grammar.Nonterminal.TopLevelStatements
import qirx.parser.grammar.Production
import qirx.parser.grammar.Sequence
import qirx.parser.grammar.Terminal
import qirx.parser.raw.RawAst
import qirx.parser.translation.ParserAdapter
import qirx.parser.translation.TerminalTranslator
import scala.language.higherKinds

class DefaultParser[T[+_]](
  // TODO for now we assume that the productions are correct, eventually we should only accept
  //      checked productions
  productions: Set[Production],
  adapter: ParserAdapter[T],
  translate: TerminalTranslator) extends Parser {

  private[this] val lookup = productions.toMap
  private[this] val translateTerminal = translate using adapter

  def parse(value: String): RawAst = {
    val start = lookup(TopLevelStatements)

    // TODO We need save a result of this, we can do so once we can serialize arbitrary runtime
    //      instances. That might take some time
    val grammar = adaptToParser(start)

    val result = adapter.parse(grammar, value)

    // TODO Think of another way to approach this
    cleanup(result)
  }

  private[this] val adaptToParser: Element => T[RawAst] = {
    case terminal: Terminal       => translateTerminal(terminal)
    case nonterminal: Nonterminal => adaptToParser(lookup(nonterminal))

    case a @ AttributedElement(element) if a.not =>
      adaptToParser(element) |> adapter.negative(raw.StringValue)

    // Note this is a naive implementation
    case Sequence(head, tail) =>
      val adaptedHead = adaptToParser(head)
      val adaptedTail = adaptToParser(tail)
      adapter.sequence(raw.Sequence.fromTuple)(adaptedHead, adaptedTail)

  }

  // TODO remove this once your head is working again
  private[this] val cleanup: RawAst => RawAst = {
    case raw.Sequence(a @ _*) =>
      raw.Sequence(
          a filter (_ != raw.NoCapture) map cleanup flatMap {
            case raw.Sequence(b @ _*) => b
            case b => Seq(b)
          }

          : _*)
    case x => x
  }

  private[this] implicit class FunctionApplyOps[A](a: A) {
    def |>[B](f: A => B): B = f(a)
  }
}
