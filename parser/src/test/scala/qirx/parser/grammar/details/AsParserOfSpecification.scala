package qirx.parser
package grammar
package details

import psp.api._
import psp.std._
import qirx.parser.grammar._
import shapeless.::
import shapeless.HNil
import utils.Documentation

object AsParserOfSpecification extends Documentation {

"""|We have defined an implicitly available type that allows us to derive the result
   |type of a parser.
   |
   |Below a list of the different available instances.
   | """.stripMargin - example {
     implicit val translateNonFree        : Translate[NonFree, String] = null
     implicit val translateFree           : Translate[Free, ExSet[Char]] = null
     implicit def translateNonTerminal[T] : Translate[Nonterminal[T], Parser[T]] = null

     def typeOfParser[E <: Element, T](e: E)(implicit ev: E AsParserOf T): T = null.asInstanceOf[T]

     case object `keyword`     extends Keyword
     case object `groupmarker` extends GroupMarker
     case object `separator`   extends Separator
     case object `feature`     extends Feature
     case object FreeValue     extends Free
     case object `scrap`       extends Scrap

     typeOfParser(`keyword`)     must be[Unit]
     typeOfParser(`groupmarker`) must be[Unit]
     typeOfParser(`separator`)   must be[Unit]
     typeOfParser(`feature`)     must be[`feature`.type]
     typeOfParser(FreeValue)     must be[String]
     typeOfParser(`scrap`)       must be[Unit]

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

     typeOfParser(`keyword`.?) must be[Unit]
     typeOfParser(`keyword`.*) must be[Unit]
     typeOfParser(`keyword`.+) must be[Unit]
   }

"""|This implicit can also be used to construct different types of results. It is required that
   |a constructor exists for these types of conversions to work.
   |
   |Below an example of the versions.
   | """.stripMargin - sideEffectExample {
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
     implicitly[Constructor[`feature`.type, ast.Ast]]
     implicitly[Constructor[`feature`.type :: HNil,  ast.Ast]]
     implicitly[Constructor[`feature`.type :: View[`feature`.type] :: HNil, ast.ViewAst]]
     implicitly[Constructor[String, ast.Number]]
   }
}
