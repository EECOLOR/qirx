package documentation

import org.qirx.littlespec.macros.Location
import org.qirx.littlespec.io.Source
import org.qirx.littlespec.fragments.Fragment
import psp.api._
import psp.std._
import psp.std.HashEq.universalEq
import qirx.parser.details._
import qirx.parser.grammar._
import qirx.parser.grammar.details.TransformedTo
import qirx.parser.grammar.details.Constructor
import qirx.parser.grammar.details.Translate
import qirx.parser.Parser
import shapeless.HList
import shapeless.HNil
import shapeless.::
import StdShow._
import utils.Documentation
import qirx.parser.Result
import qirx.parser.Position

object _04_Grammar extends Documentation {

"""|# Grammar
   |
   |A grammer consists of productions. In this particular project we focus mainly on the
   |productions of nonterminals. Our goal is to provide a facility to define production
   |of nonterminals with the minimal amount of noise.
   |
   |The grammar and the resulting AST are tied together very tighltly. This is why we
   |chose to make the grammar type safe. It makes use of some type level wizardry to
   |ensure that your grammar will fit the nonterminals.
   | """.stripMargin - {

  """|In this example we will be building a grammar that can parse something similar to
     |a function call with multiple arguments. It will be able to parse something like
     |the following:
     |
     |```scala
     |call special test("one", "two", 3)
     |\\ or
     |cass normal cubeb( 7, "eight", 9 )
     |```
     |
     |Before we can define the grammar we first need to provide the building blocks of
     |the grammar.
     |
     |## The AST
     |
     |The first set of building blocks we define are the nonterminals. But before we can
     |define those, we first need to create an AST.
     | """.stripMargin -- new Example {
       object ast {
         case class Statement(callType: Option[CallType], id:String, expressions: View[Expression])

         case class CallType(feature: Feature)

         sealed trait Expression extends Positioned
         case class StringValue(value: String) extends Expression
         case class NumberValue(value: Int) extends Expression
       }
     } chain { astExample =>

  """|## The non-string values
     |
     |As you might have noted, we have an integer in our AST.
     |
     |To be able to construct something that is an integer we have created a constructor
     |that can construct anything that takes an `Int` as argument. We have defined it
     |more generic than necessary just to show that we have already provided more generic
     |constructors.
     | """.stripMargin -- new Example {
       object constructors {
         implicit def intConstructor[A](
           implicit constructor: Constructor[Int, A]
         ): Constructor[String, A] =
           new Constructor[String, A] {
             def apply(s: Result[String]) = constructor(s map (_.toInt))
           }
       }
     } chain { constructorExample =>

     import scala.language.reflectiveCalls
     val ast = astExample.ast

  """|Note that we have extended expressions with the `Positioned` trait, that will give them
     |a position propery which we can use later on.
     |
     |## The nonterminals
     |
     |Now that we have an AST we can define the nonterminals.
     | """.stripMargin -- new Example {
       case object Statement  extends Nonterminal[ast.Statement]
       case object Expression extends Nonterminal[ast.Expression]
       case object CallType   extends Nonterminal[ast.CallType]
       case object String     extends Nonterminal[ast.StringValue]
       case object Number     extends Nonterminal[ast.NumberValue]
     } chain { nonterminals =>

  """|## The terminals
     |
     |Next up are the terminals.
     | """.stripMargin -- new Example {
       case object `(` extends GroupMarker
       case object `)` extends GroupMarker
       case object `,` extends Separator
       case object `"` extends GroupMarker // "

       case object `call`     extends Keyword
       case object `special`  extends Feature
       case object `normal`   extends Feature

       case object Id         extends Free
       case object Numeric    extends Free
       case object Whitespace extends Scrap
     } chain { terminals =>

     import terminals._

  """|## The translations
     |
     |Although we have all of the building blocks we need, there is still one thing
     |missing that is required to build a fully functional parser from a given
     |grammar: the translation of characters to terminals.
     | """.stripMargin -- new Example {
       object translations {

         val nonFreeStrings: ExMap[NonFree, String] = Direct(
           `call`    -> "call",
           `special` -> "special",
           `normal`  -> "normal",
           `(` -> "(",
           `)` -> ")",
           `,` -> ",",
           `"` -> "\""
         ).toExMap

         val freeCharacters: ExMap[Free, ExSet[Char]] = Direct(
           Id         -> ExSet('a' to 'z'),
           Numeric    -> ExSet('0' to '9'),
           Whitespace -> ExSet(Direct(' ', '\t'))
         ).toExMap
       }
     } chain { translationsExample =>

  """|## The whitespace handling
     |
     |The last thing we add is some whitespace handling.
     | """.stripMargin -- new Example {
       object whitespaceHandling {

         implicit def whitespaceHNil: (HNil TransformedTo (ZeroOrOne[Scrap] :: HNil)) =
           new (HNil TransformedTo (ZeroOrOne[Scrap] :: HNil)) {
             def apply(i: HNil) = Whitespace.? :: i
           }

         implicit def whitespaceHList[H, T <: HList, O <: HList](
           implicit transformTail: T TransformedTo O
         ): ((H :: T) TransformedTo (ZeroOrOne[Scrap] :: H :: O)) =
           new ((H :: T) TransformedTo (ZeroOrOne[Scrap] :: H :: O)) {
             def apply(i: H :: T) = Whitespace.? :: i.head :: transformTail(i.tail)
           }

         // Note that the current Scala compiler requires us to add return types to the
         // implicit methods because we import these definitions later on. In a perfect
         // world that should not be necessary because we do not add anything to the
         // signature of the trait we implement.
       }
     } chain { whitespace =>

     import translationsExample.translations
     import whitespace.whitespaceHandling
     import constructorExample.constructors
     import nonterminals._
     import terminals._

  """|Note that this seems quite a compilated way of handling whitespace. The mechanism is
     |however not designed specifically for whitespace. It allows you to modify a sequence
     |of elements before we turn it into parsers.
     |
     |This mechanisme allows you to keep your grammar clear of any stuff that could be
     |considered noise. Whitespace is such an example. A reader of my grammar intuitively
     |reads the `~` symbols as whitespace, no need to explicitly mention them there.
     |
     |## The grammar
     |
     |Now that we have all of the building blocks we can create our grammar. We do this by
     |extending the `Grammar` trait. This trait allows us to define productions in a clean
     |way. As a bonus, when we define a production, we will get a parser for free.
     | """.stripMargin -- new Example {
       object grammar extends Grammar {

         // You could add these using a trait if you wanted to keep the noise out of the grammar
         val nonFreeStrings = translations.nonFreeStrings
         val freeCharacters = translations.freeCharacters

         // import the customizations to make sure they have highest precedence
         import whitespaceHandling._
         import constructors._

         Statement  := `call` ~ CallType.? ~ Id ~ `(` ~ Expression ~ (`,` ~ Expression).* ~ `)`
         CallType   := `special` | `normal`
         Expression := String | Number
         String     := `"` ~ !`"` ~ `"` // "
         Number     := Numeric
       }
     } chain { grammarDefinition =>

     import grammarDefinition.grammar

  """|Note that behind the scenes a mutable variable is used to store the productions, this
     |allows us to define productions in a cleaner fashion. Using a mutable variable here is
     |perfectly safe because we only use it in the constructor and do not expose it to the
     |outside world.
     |
     |## The parsers
     |
     |The above grammar has produced parsers that are useful for us. These are the parsers of
     |nonterminals and with that our AST.
     | """.stripMargin -- new Example {
       val Some(statementParser  : Parser[ast.Statement  ]) = grammar parserFor Statement
       val Some(callTypeParser   : Parser[ast.CallType   ]) = grammar parserFor CallType
       val Some(expressionParser : Parser[ast.Expression ]) = grammar parserFor Expression
       val Some(stringParser     : Parser[ast.StringValue]) = grammar parserFor String
       val Some(numberParser     : Parser[ast.NumberValue]) = grammar parserFor Number
     } chain { parsers =>

     import parsers.statementParser
     implicit val showExpression = Show.natural[ast.Expression]

  """|## The result
     |
     |Each of the parsers will be able to parse their part of the grammar. Since the `Statement`
     |parser uses all of the other parsers, we will only use that one in this example.
     |
     |Lets see the fruits of our labor.
     | """.stripMargin - example {
       val result =  statementParser parse """  call special  test("test1" ,  "test2",12) """

       result match {
         case Failed(cause) => failure(cause.toString)

         case Succeeded(results) if results.size == Size(1L) =>
           val result = results.head
           val statement = result.value

           statement.callType is Some(ast.CallType(`special`))
           statement.id is "test"

           val expressions = statement.expressions.toDirect
           expressions.to_s is Direct(
             ast.StringValue("test1"),
             ast.StringValue("test2"),
             ast.NumberValue(12)
           ).to_s

           expressions(Index(0)).position is Position(22, 27)
           expressions(Index(1)).position is Position(33, 38)
           expressions(Index(2)).position is Position(40, 42)

         case Succeeded(results) => failure("Expected a single result, got " + results.size + " results")
       }
     }}}}}}}}} // The above examples are nested so that parts can be reused, we chose to visually
               // hide that fact, resulting in these closing braces
   }
}