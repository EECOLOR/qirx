package qirx.parser.scalaparsercombinators

import scala.util.parsing.combinator.Parsers
import qirx.parser.translation.ParserAdapter
import scala.util.parsing.input.CharSequenceReader

object ScalaParserCombinatorsParserAdapter extends Parsers {

  type Elem = Char

  val any = acceptIf(_ => true)(_ => ???)

  object `I hate it when people force me to do this` extends ParserAdapter[Parser] {

    def predicate[X](f: String => X)(predicate: Char => Boolean): Parser[X] =
      rep(acceptIf(predicate)(c => s"Char `$c` was not accepted")) ^^ (c => f(c.mkString))

    def char[X](f: String => X)(char: Char): Parser[X] = char ^^ (c => f(c.toString))

    def negative[X](f: String => X)(element: Parser[_]): Parser[X] =
      (not(element) ~ any).* ^^ { l => f(l.map { case _ ~ x => x }.mkString) }

    def sequence[X, Y](f: ((X, X)) => Y)(head: Parser[X], tail: Parser[X]): Parser[Y] =
      head ~ tail ^^ {case x1 ~ x2 => f(x1 -> x2) }

    def parse[X](adaptedGrammar: Parser[X], value: String): X =
      adaptedGrammar(new CharSequenceReader(value)) match {
        case Success(result, _) => result
        case Failure(msg, next) => sys.error(msg + next)
        case Error(msg, next)   => sys.error(msg + next)
      }
  }
}
