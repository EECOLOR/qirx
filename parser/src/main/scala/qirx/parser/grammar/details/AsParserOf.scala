package qirx.parser
package grammar.details

import psp.api._
import psp.std._
import shapeless.::
import shapeless.HNil
import shapeless.HList
import qirx.parser.grammar._
import qirx.parser.Parser
import qirx.parser.parsers._
import qirx.parser.Failure

trait AsParserOf[-E <: Element, O] {
  def apply(e:E):Parser[O]
}

object AsParserOf {

  import utilities._

  private def characterParserFor[A](
    characters : ExSet[Char],
    toValue    : InvariantView[Char] with HasPreciseSize => A
  ): Parser[A] =
    CharacterParser(_ span characters toOutcome s"Expected one of ${characters mkString ""}", toValue)

  implicit def variableString(
    implicit variableCharacters : Translate[Variable, ExSet[Char]]
  ) =
    new ((Variable with Capture[String]) AsParserOf String) {
      def apply(e: Variable with Capture[String]) = characterParserFor(variableCharacters(e), _.force)
    }

  implicit def variableUnit(
    implicit variableCharacters : Translate[Variable, ExSet[Char]]
  ) =
    new ((Variable with Capture[Unit]) AsParserOf Unit) {
      def apply(e: Variable with Capture[Unit]) = characterParserFor(variableCharacters(e), _ => ())
    }

  implicit def fixedUnit(
    implicit fixedStrings: Translate[Fixed, String]
  ) =
    new ((Fixed with Capture[Unit]) AsParserOf Unit) {
      def apply(e: Fixed with Capture[Unit]) = CharacterParser.string(fixedStrings(e), _ => ())
    }

  implicit def fixedSelf[T <: Fixed with Capture[Capture.Self]](
    implicit fixedStrings : Translate[Fixed, String]
  ) =
    new (T AsParserOf T) {
      def apply(e: T) = CharacterParser.string(fixedStrings(e), _ => e)
    }

  implicit def nonterminal[T](
    implicit nonTerminalParsers: Translate[Nonterminal[T], Parser[T]]
  ) =
    new (Nonterminal[T] AsParserOf T) {
      def apply(e: Nonterminal[T]) = nonTerminalParsers(e)
    }

  implicit def not[A <: Element, B](
    implicit asParser    : A AsParserOf B
  ) =
    new (Not[A] AsParserOf String) {
      def apply(e: Not[A]) = NotParser(asParser(e.element), _.force)
    }

  implicit def zeroOrOne [A <: Element, B, C](
    implicit asParser    : A AsParserOf B,
             normalize   : Option[B] NormalizedAs C
  ) =
    new (ZeroOrOne[A] AsParserOf C) {
      def apply(e: ZeroOrOne[A]) = ZeroOrOneParser(asParser(e.element), normalize)
    }

  implicit def zeroOrMore[A <: Element, B, C](
    implicit asParser    : A AsParserOf B,
             normalize   : View[Result[B]] NormalizedAs C
  ) =
    new (ZeroOrMore[A] AsParserOf C) {
      def apply(e: ZeroOrMore[A]) = ZeroOrMoreParser(asParser(e.element), normalize)
    }

  implicit def oneOrMore[A <: Element, B, C](
    implicit asParser    : A AsParserOf B,
             normalize   : View[Result[B]] NormalizedAs C
  ) =
    new (OneOrMore[A] AsParserOf C) {
      def apply(e: OneOrMore[A]) = OneOrMoreParser(asParser(e.element), normalize)
    }

  import SequenceParser.ParsesTo

  implicit def sequence[E <: HList, F <: HList, H, T <: HList, A <: HList, B <: HList, C, D](
    implicit constomize    : E TransformedTo F,
             asParsers     : F AsParserList (H :: T),
             parse         : (H :: T) ParsesTo A,
             withoutUnit   : A WithoutUnitAs B,
             normalize     : B NormalizedAs C,
             flatten       : C FlattenedTo D
  ) =
    new (Sequence[E] AsParserOf D) {
      def apply(e: Sequence[E]) = {
        val parser =
          SequenceParser(
            e.elements |> constomize |> asParsers,
            withoutUnit andThen normalize
          )

        new Parser[D] {
          def parse(input: Input): Failure | View[Result[D]] =
            parser parse input mapResult flatten
        }
      }
    }

  implicit def choice[E <: HList, A, T <: HList](
    implicit asParsers   : E AsParserList T,
             parserTypes : T HasCommonSuperTypeOf Parser[A],
             toView      : T ToViewOf Parser[A]
  ) =
    new (Choice[E] AsParserOf A) {
      def apply(e: Choice[E]) = ChoiceParser(e.options |> asParsers |> toView, identity[A])
    }

  object utilities {

    // Utility traits are marked as sealed because we do not want them to be implemented by the
    // public. They are an implementation detail.

    sealed trait AsParserList[-E <: HList, P <: HList] extends (E => P) {
      def apply(list:E): P
    }

    object AsParserList {

      implicit def hnil =
        new (HNil AsParserList HNil) {
          def apply(list: HNil) = list
        }

      implicit def hlist[H <: Element, T <: HList, A, P <: HList](
        implicit asHeadParser  : H AsParserOf A,
                 asTailParsers : T AsParserList P
      ) =
        new ((H :: T) AsParserList (Parser[A] :: P)) {
          def apply(list: H :: T) = asHeadParser(list.head) :: asTailParsers(list.tail)
        }
    }

    sealed trait FlattenedTo[A, B] extends (Result[A] => Result[B]) {
      def apply(a: Result[A]):Result[B]
    }

    sealed trait LowerPriorityFlattenedTo {

      implicit def any[A] =
        new (A FlattenedTo A) {
          def apply(a: Result[A]) = a
        }
    }

    object FlattenedTo extends LowerPriorityFlattenedTo {

      implicit def result[A] =
        new (Result[A] FlattenedTo A) {
          // drop outer result
          def apply(a: Result[Result[A]]) = {
            val Result(Result(value, position, _), _, remaining) = a

            Result(value, position, remaining)
          }
        }
    }

    sealed trait ToViewOf[-L <: HList, +A] extends (L => View[A]) {
      def apply(list: L): View[A]
    }

    object ToViewOf {

      implicit def hnil[A] =
        new (HNil ToViewOf A) {
          def apply(p:HNil) = emptyValue[View[A]]
        }

      implicit def hlist[H, T <: HList, A](
        implicit headConforms : H <:< A,
                 tailToView   : T ToViewOf A
      ) =
        new ((H :: T) ToViewOf A) {
          def apply(list: H :: T) = list.head +: tailToView(list.tail)
        }
    }

    sealed trait HasCommonSuperTypeOf[-A, +O]

    sealed trait LowerPriorityHasCommonSuperTypeOf {

      implicit def multipleElements[H, T <: HList, A, B](
        implicit tail         : T HasCommonSuperTypeOf A,
                 headWithTail : (H, A) HasCommonSuperTypeOf B
      ): (H :: T) HasCommonSuperTypeOf B = null
    }

    object HasCommonSuperTypeOf extends LowerPriorityHasCommonSuperTypeOf {

      implicit def twoElements[A, B, C](
        implicit ev: (A, B) HasCommonSuperTypeOf C
      ): (A :: B :: HNil) HasCommonSuperTypeOf C = null

      implicit def tuple[A]: (A, A) HasCommonSuperTypeOf A = null
    }

    sealed trait WithoutUnitAs[-L <: HList, O <: HList] extends (L => O) {
      def apply(list: L):O
    }

    sealed trait LowerPriorityWithoutUnitAs {

      implicit def hlist[H, T <: HList, O <: HList](
        implicit tailWithoutUnit: T WithoutUnitAs O
      ) =
        new ((H :: T) WithoutUnitAs (H :: O)) {
          def apply(list: H :: T) = list.head :: tailWithoutUnit(list.tail)
        }
    }

    object WithoutUnitAs extends LowerPriorityWithoutUnitAs {

      implicit def hnil =
        new (HNil WithoutUnitAs HNil) {
          def apply(list: HNil) = list
        }

      implicit def result[T <: HList, O <: HList](
        implicit tailWithoutUnit: T WithoutUnitAs O
      ) =
        new ((Result[Unit] :: T) WithoutUnitAs O) {
          def apply(list: Result[Unit] :: T) = tailWithoutUnit(list.tail)
        }
    }

    sealed trait NormalizedAs[-I, O] extends (I => O) {
      def apply(i:I):O
    }

    sealed trait LowerPriorityNormalizedAs {

      implicit def any[T] =
        new (T NormalizedAs T) {
          def apply(i: T) = i
        }
    }

    object NormalizedAs extends LowerPriorityNormalizedAs {

      implicit def hnil =
        new (HNil NormalizedAs Unit) {
          def apply(i: HNil) = ()
        }

      implicit def unitResult[A](
        implicit normalized: A NormalizedAs Unit
      ) =
        new (Result[A] NormalizedAs Unit) {
          def apply(i: Result[A]) = ()
        }

      implicit def single[H] =
        new ((H :: HNil) NormalizedAs H) {
          def apply(i: H :: HNil) = i.head
        }

      implicit def unitOption[A](
        implicit normalized: A NormalizedAs Unit
      ) =
        new (Option[A] NormalizedAs Unit) {
          def apply(i: Option[A]) = ()
        }

      implicit def unitView[A](
        implicit normalized: A NormalizedAs Unit
      ) =
        new (View[A] NormalizedAs Unit) {
          def apply(i: View[A]) = ()
        }
    }
  }
}
