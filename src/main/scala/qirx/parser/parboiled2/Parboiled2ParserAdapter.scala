package qirx.parser.parboiled2

import org.parboiled2.CharPredicate
import org.parboiled2.Parser
import org.parboiled2.Parser.DeliveryScheme.Throw
import org.parboiled2.ParserInput
import org.parboiled2.Rule1
import qirx.parser.translation.ParserAdapter

// Well... this doesn't work...
object Parboiled2ParserAdapter extends Parser with ParserAdapter[Rule1] {

  var parserInput: Option[ParserInput] = None

  def input:ParserInput = parserInput.getOrElse(sys.error("No parser input available"))

  def predicate[X](f: String => X)(p: Char => Boolean):Rule1[X] =
    rule { capture( zeroOrMore(CharPredicate.from(p)) ) ~> f }

  def char[X](f: String => X)(char:Char):Rule1[X] =
    rule { capture(char) ~> f }

  def negative[X](f: String => X)(element:Rule1[_]):Rule1[X] =
    rule { capture( zeroOrMore( !element ~ ANY ) ) ~> f }

  def sequence[X, Y](f: ((X, X)) => Y)(head: Rule1[X], tail: Rule1[X]):Rule1[Y] =
    rule { (head ~ tail) ~> ((x1:X, x2:X) => f(x1 -> x2)) }

  def parse[X](adaptedGrammar: Rule1[X], value:String):X = {
    parserInput = Some(ParserInput(value))
    println(adaptedGrammar)
    val result = __run(adaptedGrammar)
    parserInput = None
    result
  }
}
