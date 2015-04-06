**This documentation is generated from `documentation._03_Productions`**

---
# Productions

A grammer consists of productions. In this particular project we focus mainly on the
productions of nonterminals. Our goal is to provide a facility to define production
with the minimal amount of noise.

The grammar and the resulting AST are tied together very tighltly. This is why we
chose to make the grammar type safe. It makes use of some type level wizardry to
ensure that your grammar will fit the nonterminals.
 
We have defined an implicitly available type that allows us to derive the result
type of a parser.
 
```scala
implicit val translateNonFree        : Translate[NonFree, String] = null
implicit val translateFree           : Translate[Free, ExSet[Char]] = null
implicit def translateNonTerminal[T] : Translate[Nonterminal[T], Parser[T]] = null

def typeOfParser[E <: Element, T](e: E)(implicit ev: E AsParserOf T): T = null.asInstanceOf[T]

case object `keyword`     extends Keyword
case object `groupmarker` extends GroupMarker
case object `separator`   extends Separator
case object `feature`     extends Feature
case object FreeValue     extends Free

typeOfParser(`keyword`)     must be[Unit]
typeOfParser(`groupmarker`) must be[Unit]
typeOfParser(`separator`)   must be[Unit]
typeOfParser(`feature`)     must be[`feature`.type]
typeOfParser(FreeValue)     must be[String]

object NonterminalValue extends Nonterminal[Boolean]
typeOfParser(NonterminalValue) must be[Boolean]

typeOfParser(!`feature`)  must be[String]
typeOfParser(`feature`.?) must be[Option[`feature`.type]]
typeOfParser(`feature`.*) must be[View[`feature`.type]]
typeOfParser(`feature`.+) must be[View[`feature`.type]]

trait MyFeature extends Feature
case object `feature1` extends MyFeature
case object `feature2` extends MyFeature
typeOfParser(`feature1` | `feature2`) must be[MyFeature]

trait MyAst
trait Ast1 extends MyAst
trait Ast2 extends MyAst
object AstNonterminal1 extends Nonterminal[Ast1]
object AstNonterminal2 extends Nonterminal[Ast2]
typeOfParser(AstNonterminal1 | AstNonterminal2) must be[MyAst]
typeOfParser(AstNonterminal1 ~ AstNonterminal2 | AstNonterminal1 ~ AstNonterminal2) must be[Ast1 :: Ast2 :: HNil]
typeOfParser(AstNonterminal2 ~ AstNonterminal1 | AstNonterminal1 ~ AstNonterminal2) must be[MyAst :: MyAst :: HNil]
typeOfParser(AstNonterminal1 | AstNonterminal2 | AstNonterminal1) must be[MyAst]
typeOfParser(AstNonterminal1 | AstNonterminal1 | AstNonterminal1) must be[Ast1]
typeOfParser(NonterminalValue) must be[Boolean]
typeOfParser(`keyword` ~ NonterminalValue) must be[Boolean]
typeOfParser(`keyword` ~ `groupmarker` ~ `separator`) must be[Unit]
typeOfParser(NonterminalValue ~ `groupmarker` ~ FreeValue ~ `separator` ~ `feature`) must
  be[Boolean :: String :: `feature`.type :: HNil]
```
This implicit can also be used to construct different types of results. It is required that
a constructor exists for these types of conversions to work.
 
```scala
implicit val translateNonFree        : Translate[NonFree, String] = null
implicit val translateFree           : Translate[Free, ExSet[Char]] = null
implicit def translateNonTerminal[T] : Translate[Nonterminal[T], Parser[T]] = null

case object `feature` extends Feature

object ast {
  case class Ast(element: Feature)
  case class ViewAst(elements: View[Feature])
  case class Number(value: Int)
  object Number {
    implicit def intValue[B](implicit construct: Constructor[Int, B]):Constructor[String, B] =
      new Constructor[String, B] {
        def apply(a:String) = construct(a.toInt)
      }
  }
}
implicitly[`feature`.type AsParserOf Feature]
implicitly[`feature`.type AsParserOf ast.Ast]
implicitly[Sequence[`feature`.type :: HNil] AsParserOf ast.Ast]
implicitly[Sequence[`feature`.type :: ZeroOrMore[`feature`.type] :: HNil] AsParserOf ast.ViewAst]
implicitly[Not[`feature`.type] AsParserOf ast.Number]
```
To define a set of productions you can extend the grammar trait. This trait allows you
to define productions in a clean way. When you define a production, you will get a
parser for free.

Note that behind the screens a variable is used to the productions. This is perfectly
safe because we only use it in the constructor and do not expose it to the outside
world.

In the following example we define a grammar that accepts arbitrary methods with
one or more strings or numbers as argument.
 
```scala
// Create the grammar
object grammar extends Grammar with Translations {
  Statement  := `call` ~ CallType.? ~ Id ~ `(` ~ Expression ~ (`,` ~ Expression).* ~ `)`
  CallType   := `special` | `normal`
  Expression := String | Number
  String     := `"` ~ !`"` ~ `"` // "
  Number     := Numeric
}

// Define the nonterminals
case object Statement  extends Nonterminal[ast.Statement]
case object Expression extends Nonterminal[ast.Expression]
case object CallType   extends Nonterminal[ast.CallType]
case object String     extends Nonterminal[ast.StringValue]
case object Number     extends Nonterminal[ast.NumberValue]

// Define the AST
object ast {
  case class Statement(callType: Option[CallType], id:String, expressions: View[Expression])

  case class CallType(feature: Feature)

  sealed trait Expression
  case class StringValue(value: String) extends Expression
  case class NumberValue(value: Int) extends Expression

  object NumberValue {
    // We need to define the constructor near the resulting type to avoid ambigiuty when
    // searching for the implicits
    implicit def intConstructor[A](implicit constructor: Constructor[Int, A]): Constructor[String, A] =
      new Constructor[String, A] {
        def apply(s: String) = constructor(s.toInt)
      }
  }
}

// Define the terminals
case object `(` extends GroupMarker
case object `)` extends GroupMarker
case object `,` extends Separator
case object `"` extends GroupMarker // "
case object `call`    extends Keyword
case object `special` extends Feature
case object `normal`  extends Feature
case object Id        extends Free
case object Numeric   extends Free

// Define the translations
trait Translations extends NonFreeStrings with FreeCharacters {
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
    Id      -> ExSet('a' to 'z'),
    Numeric -> ExSet('0' to '9')
  ).toExMap
}

val Some(parser) = grammar.parser(Statement)
parser parse """callspecialtest("test1","test2",12)""" match {
  case Left(f) => failure(f.toString)
  case Right(successes) if successes.size == Size(1L) =>
    val result = successes.head
    val statement = result.value
    statement.callType is Some(ast.CallType(`special`))
    statement.id is "test"

    import StdShow._

    implicit val s = Show.natural[ast.Expression]

    statement.expressions.to_s is Direct(
      ast.StringValue("test1"),
      ast.StringValue("test2"),
      ast.NumberValue(122)
    ).to_s
  case Right(successes) => failure("Expected a single result, got " + successes.size + " results")
}
```
> Failure: [ StringValue(test1), StringValue(test2), NumberValue(12) ] is not equal to [ StringValue(test1), StringValue(test2), NumberValue(122) ]

