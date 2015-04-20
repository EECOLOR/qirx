package qirx.parser
package grammar
package details

import psp.api._
import psp.std._
import qirx.parser.grammar._
import shapeless.::
import shapeless.HNil
import utils.Documentation
import shapeless.HList
import shapeless.Generic

object ProductionSpecification extends Documentation {

  "Here I have tried to list most combinations of productions." - {

    object TestGrammar extends Grammar {

      // This is about compiling, no need to provide real values
      def variableCharacters = null
      def fixedStrings = null

      trait CustomFeature extends Fixed with Capture[Capture.Self]
      object ast {
        case class CombinedValue(
          a: String,
          b: CustomFeature,
          c: Option[String],
          d: View[String],
          e: CombinedValue
        )
      }

      object UnitValue     extends Nonterminal[Unit]
      object StringValue   extends Nonterminal[String]
      object CustomFeature extends Nonterminal[CustomFeature]
      object OptionValue   extends Nonterminal[Option[String]]
      object ViewValue     extends Nonterminal[View[String]]
      object CombinedValue extends Nonterminal[ast.CombinedValue]

      val fixedUnit: Fixed with Capture[Unit] = null
      val variableString: Variable with Capture[String] = null
      val variableUnit: Variable with Capture[Unit] = null
      val customFeature: CustomFeature = null

      UnitValue     := variableUnit | fixedUnit | fixedUnit.? | fixedUnit.+ | fixedUnit.* | fixedUnit ~ fixedUnit | UnitValue
      StringValue   := variableString | !fixedUnit | fixedUnit ~ variableString | StringValue
      CustomFeature := customFeature | CustomFeature
      OptionValue   := variableString.? | OptionValue
      ViewValue     := variableString.+ | variableString.*
      ViewValue     := ViewValue
      ViewValue     := variableString ~ variableString.*
      CombinedValue := StringValue ~ CustomFeature ~ OptionValue ~ ViewValue ~ CombinedValue
      CombinedValue := CombinedValue
    }

    success
  }

  "Customization helpers" - {
    trait Test extends Scrap
    object Test extends Test
    trait Test2 extends Scrap
    object Test2 extends Test2

    implicit def transformSingle[X]: (Test TransformedTo Test2)#InContext[X] =
      new (Test TransformedTo Test2) {
        def apply(i: Test) = null
      }.InContext[X]

    val zeroOrMore = implicitly[(ZeroOrMore[Test] TransformedTo ZeroOrMore[Test2])#InContext[Any]]
    zeroOrMore(Test.*) is ZeroOrMore(null)

    val oneOrMore = implicitly[(OneOrMore[Test] TransformedTo OneOrMore[Test2])#InContext[Any]]
    oneOrMore(Test.+) is OneOrMore(null)

    val zeroOrOne = implicitly[(ZeroOrOne[Test] TransformedTo ZeroOrOne[Test2])#InContext[Any]]
    zeroOrOne(Test.?) is ZeroOrOne(null)

    val not = implicitly[(Not[Test] TransformedTo Not[Test2])#InContext[Any]]
    not(!Test) is Not(null)

    implicit def transformHList[X](
      implicit transform: (Test TransformedTo Test2)#InContext[X]
    ): ((Test :: HNil) TransformedTo (Test2 :: HNil))#InContext[X] =
      new ((Test :: HNil) TransformedTo (Test2 :: HNil)) {
        def apply(i: Test :: HNil) = transform(i.head) :: HNil
      }.InContext[X]

    val sequence = implicitly[(Sequence[Test :: HNil] TransformedTo Sequence[Test2 :: HNil])#InContext[Any]]
    sequence(Sequence(Test :: HNil)) is Sequence(null :: HNil)

    val choice = implicitly[(Choice[Test :: HNil] TransformedTo Choice[Test2 :: HNil])#InContext[Any]]
    choice(Choice(Test :: HNil)) is Choice(null :: HNil)
  }

  "Unit tests for the stuff that deals with results" - {

     trait CustomType1
     trait CustomType2
     trait CustomPositioned extends Positioned
     object ast {
       case class CustomCasePositioned(value: CustomType1) extends Positioned
     }

     "AsParserOf.utilities.FlattenedTo" - {

       import AsParserOf.utilities.FlattenedTo
       val customInstance = new CustomType1 {}
       val remaining: Input = "b"
       val target = Result(Result(customInstance, Position(1, 2), "a"), Position(2, 3), remaining)

       val flatten = implicitly[Result[CustomType1] FlattenedTo CustomType1]

       flatten(target) is Result(customInstance, Position(1, 2), remaining)
     }

     "Constructor.utilities.AddPositionsTo" - {
       import Constructor.utilities.AddPositionsTo

       val addPositions = implicitly[AddPositionsTo[CustomPositioned]]
       val instance = new CustomPositioned {}
       val position = Position(1, 2)

       addPositions(instance, position)

       instance.position is Position(1, 2)
     }

     "Constructor.utilities.SimplifiedAs" - {
       import Constructor.utilities.SimplifiedAs

       val customInstance1 = new CustomType1 {}
       val result1 = Result(new CustomType2 {}, Position(1, 2), "a")
       val result2 = Result(new CustomType2 {}, Position(4, 5), "b")
       val target =
         customInstance1 ::
         result1 ::
         Result(newView(result2), Position(3, 6), "c") ::
         HNil

       val simplify = implicitly[(CustomType1 :: Result[CustomType2] :: Result[View[Result[CustomType2]]] :: HNil) SimplifiedAs (CustomType1 :: Result[View[Result[CustomType2]]] :: HNil)]

       val result = simplify(target)
       val firstValue = result.head
       val secondValue = result.tail.head
       val Result(resultView, position, remaining) = secondValue
       val directResult = resultView.toDirect

       firstValue is customInstance1
       position is Position(1, 6)
       remaining.underlying.force[String] is "c"
       directResult(Index(0)) is result1
       directResult(Index(1)) is result2
     }

     "Constructor" - {

       "Exact" - {
         val instance = new CustomPositioned {}
         val position = Position(1, 2)
         val construct = implicitly[Constructor[CustomPositioned, CustomPositioned]]

         val result = construct(Result(instance, position, ""))
         result is instance
         result.position is position
       }

       "Case class like" - {
         val instance = new CustomType1 {}
         val position = Position(1, 2)
         val construct = implicitly[Constructor[CustomType1, ast.CustomCasePositioned]]

         val result = construct(Result(instance, position, ""))
         val ast.CustomCasePositioned(value) = result
         value is instance
         result.position is position
       }
     }
  }
}
