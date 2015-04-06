package qirx.parser.grammar.details

import psp.api._
import psp.std._
import shapeless.::
import shapeless.HNil
import shapeless.HList
import qirx.parser.details.ParsesTo
import qirx.parser.grammar._
import qirx.parser.Parser
import qirx.parser.parsers._

trait AsParserOf[-E <: Element, O] {
  def apply(e:E):Parser[O]
}

trait LowerPriorityAsParserOf {
  implicit def nonFree(
    implicit nonFreeStrings: Translate[NonFree, String]
  ) =
    new (NonFree AsParserOf Unit) {
      def apply(e: NonFree) = CharacterParser.string(nonFreeStrings(e), _ => ())
    }

  implicit def free[A](
    implicit freeCharacters : Translate[Free, ExSet[Char]],
      constructor    : Constructor[String, A]
    ) =
      new (Free AsParserOf A) {
        def apply(e: Free) = CharacterParser(_ span freeCharacters(e), constructor compose (_.force))
      }
}

object AsParserOf extends LowerPriorityAsParserOf {

  import utilities._

  implicit def scrap[A](
    implicit freeCharacters : Translate[Free, ExSet[Char]]
  ) =
    new (Scrap AsParserOf Unit) {
      def apply(e: Scrap) = CharacterParser(_ span freeCharacters(e), _ => ())
    }

  implicit def not[A <: Element, B, C](
    implicit asParser    : A AsParserOf B,
             constructor : Constructor[String, C]
  ) =
    new (Not[A] AsParserOf C) {
      def apply(e: Not[A]) = NotParser(asParser(e.element), constructor compose (_.force))
    }

  implicit def feature[T <: Feature, A](
    implicit nonFreeStrings : Translate[Feature, String],
             constructor    : Constructor[T, A]
  ) =
    new (T AsParserOf A) {
      def apply(e: T) = CharacterParser.string(nonFreeStrings(e), _ => constructor(e))
    }

  implicit def nonterminal[T](
    implicit nonTerminalParsers: Translate[Nonterminal[T], Parser[T]]
  ) =
    new (Nonterminal[T] AsParserOf T) {
      def apply(e: Nonterminal[T]) = nonTerminalParsers(e)
    }

  implicit def zeroOrOne [A <: Element, B, C, D](
    implicit asParser    : A AsParserOf B,
             normalize   : Option[B] NormalizedAs C,
             constructor : Constructor[C, D]
  ) =
    new (ZeroOrOne[A] AsParserOf D) {
      def apply(e: ZeroOrOne[A]) = ZeroOrOneParser(asParser(e.element), normalize andThen constructor)
    }

  implicit def zeroOrMore[A <: Element, B, C, D](
    implicit asParser    : A AsParserOf B,
             normalize   : View[B] NormalizedAs C,
             constructor : Constructor[C, D]
  ) =
    new (ZeroOrMore[A] AsParserOf D) {
      def apply(e: ZeroOrMore[A]) = ZeroOrMoreParser(asParser(e.element), normalize andThen constructor)
    }

  implicit def oneOrMore[A <: Element, B, C, D](
    implicit asParser    : A AsParserOf B,
             normalize   : View[B] NormalizedAs C,
             constructor : Constructor[C, D]
  ) =
    new (OneOrMore[A] AsParserOf D) {
      def apply(e: OneOrMore[A]) = OneOrMoreParser(asParser(e.element), normalize andThen constructor)
    }

  implicit def sequence[E <: HList, F <: HList, H, T <: HList, A <: HList, B <: HList, C, D](
    implicit constomize  : E TransformedTo F,
             asParsers   : F AsParserList (H :: T),
             parse       : (H :: T) ParsesTo A,
             withoutUnit : A WithoutUnitAs B,
             normalize   : B NormalizedAs C,
             constructor : Constructor[C, D]
  ) =
    new (Sequence[E] AsParserOf D) {
      def apply(e: Sequence[E]) =
        SequenceParser(asParsers(e.elements), withoutUnit andThen normalize andThen constructor)
    }

  implicit def choice[E <: HList, A, T <: HList, B, C](
    implicit asParsers   : E AsParserList T,
             parserTypes : T HasCommonSuperTypeOf Parser[A],
             toView      : T ToViewOf Parser[A],
             constructor : Constructor[A, B]
  ) =
    new (Choice[E] AsParserOf B) {
      def apply(e: Choice[E]) = ChoiceParser(e.options |> asParsers |> toView, constructor)
    }

  object utilities {

    trait ToViewOf[-L <: HList, +A] extends (L => View[A]) {
      def apply(list: L): View[A]
    }

    object ToViewOf {
      implicit def hnil[A] =
        new (HNil ToViewOf A) {
          def apply(p:HNil) = emptyValue[View[A]]
        }

      implicit def hlist[H, T <: HList, A](
        implicit headConforms: H <:< A,
                 tailToView: T ToViewOf A
      ) =
        new ((H :: T) ToViewOf A) {
          def apply(list: H :: T) = list.head +: tailToView(list.tail)
        }
    }

    trait HasCommonSuperTypeOf[-A, +O]

    trait LowerPriorityHasCommonSuperTypeOf {

      implicit def multipleElements[H, T <: HList, A, B](
        implicit tail: T HasCommonSuperTypeOf A,
                 headWithTail: (H, A) HasCommonSuperTypeOf B
      ): (H :: T) HasCommonSuperTypeOf B = null
    }

    object HasCommonSuperTypeOf extends LowerPriorityHasCommonSuperTypeOf {

      implicit def twoElements[A, B, C](
        implicit ev: (A, B) HasCommonSuperTypeOf C
      ): (A :: B :: HNil) HasCommonSuperTypeOf C = null

      implicit def tuple[A]: (A, A) HasCommonSuperTypeOf A = null
    }

    trait WithoutUnitAs[-L <: HList, O <: HList] extends (L => O) {
      def apply(list: L):O
    }

    trait LowerPriorityWithoutUnitAs {
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

      implicit def hlistWithUnit[T <: HList, O <: HList](
        implicit tailWithoutUnit: T WithoutUnitAs O
      ) =
        new ((Unit :: T) WithoutUnitAs O) {
          def apply(list: Unit :: T) = tailWithoutUnit(list.tail)
        }
    }

    trait NormalizedAs[-I, O] extends (I => O) {
      def apply(i:I):O
    }

    trait LowerPriorityNormalizedAs {
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

      implicit def single[H] =
        new ((H :: HNil) NormalizedAs H) {
          def apply(i: H :: HNil) = i.head
        }

      implicit def unitOption =
        new (Option[Unit] NormalizedAs Unit) {
          def apply(i: Option[Unit]) = ()
        }

      implicit def unitView =
        new (View[Unit] NormalizedAs Unit) {
          def apply(i: View[Unit]) = ()
        }
    }
  }
}
