package qirx.parser.gllcombinators

import qirx.parser.translation.ParserAdapter
import com.codecommit.gll.Parsers._
import com.codecommit.util.UniversalOptCharSet
import com.codecommit.gll.LineStream
import com.codecommit.gll.Result
import com.codecommit.gll.Success
import com.codecommit.gll.Failure
import com.codecommit.gll.UnexpectedEndOfStream

// Not working...
object GllCombinatorsParserAdapter extends ParserAdapter[Parser] {

  val any = new TerminalParser[Char] {
    def computeFirst(seen: Set[Parser[Any]]): Option[Set[Option[Char]]] = Some(UniversalOptCharSet)
    def parse(in: LineStream): Result[Char] = {
      val s = in take 1
      if (s.length == 1) Success(s charAt 0, s drop 1)
      else Failure(UnexpectedEndOfStream(None), in)
    }
  }

  def predicate[X](f: String => X)(predicate: Char => Boolean): Parser[X] =
    any.filter(predicate).* ^^ { c => f(c.mkString) }

  def char[X](f: String => X)(char: Char): Parser[X] =
    char.toString ^^ f

  def negative[X](f: String => X)(element: Parser[_]): Parser[X] =
    (any \ element.asInstanceOf[TerminalParser[Any]]).* ^^ (l => f(l.mkString))

  def sequence[X, Y](f: ((X, X)) => Y)(head: Parser[X], tail: Parser[X]): Parser[Y] =
    head ~ tail ^^ ((x1, x2) => f(x1 -> x2))

  def parse[X](adaptedGrammar: Parser[X], value: String): X =
    adaptedGrammar(value).head match {
      case Success(value, _) => value
      case Failure(data, t)  => sys.error(data.toString + " : " + t)
    }

}
