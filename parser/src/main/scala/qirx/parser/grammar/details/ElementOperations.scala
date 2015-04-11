package qirx.parser.grammar
package details

import shapeless.HList
import shapeless.HNil
import shapeless.ops.hlist.Prepend
import shapeless.::

trait ElementOperations  {
  import utilities._

  implicit class ElementOperations[A <: Element](a:A) {
    def ~ [B](b: B)(implicit append:Append[A, B]) = append(a, b)
    def | [B](b: B)(implicit choose:Choose[A, B]) = choose(a, b)

    def       + = OneOrMore(a)
    def       * = ZeroOrMore(a)
    def       ? = ZeroOrOne(a)
    def unary_! = Not(a)
  }

  object utilities {

    abstract class Append[A, B] {
      type Elements <: HList
      def apply(a:A, b:B): Sequence[Elements]
    }

    trait LowerPriorityAppend0 {

      implicit def `e ~ e`[E1, E2](
        implicit ev: (E1 :: E2 :: HNil) ContainsSubTypesOf Element
      ) =
        new Append[E1, E2] {
          type Elements = E1 :: E2 :: HNil
          def apply(a:E1, b:E2) = Sequence(a :: b :: HNil)
        }
    }

    trait LowerPriorityAppend1 extends LowerPriorityAppend0 {

      implicit def `e ~ s`[E1, E2 <: HList](
        implicit ev: (E1 :: E2) ContainsSubTypesOf Element
      ) =
        new Append[E1, Sequence[E2]] {
          type Elements = E1 :: E2
          def apply(a:E1, b: Sequence[E2]) = Sequence(a :: b.elements)
        }

      implicit def `s ~ e`[E1 <: HList, E2, E <: HList](
        implicit prepend : Prepend.Aux[E1, E2 :: HNil, E],
                 ev      : E ContainsSubTypesOf Element
       ) =
        new Append[Sequence[E1], E2] {
          type Elements = E
          def apply(a:Sequence[E1], b: E2) = Sequence(prepend(a.elements, b :: HNil))
        }
    }

    object Append extends LowerPriorityAppend1 {

      implicit def `s ~ s`[E1 <: HList, E2 <: HList, E <: HList](
        implicit prepend : Prepend.Aux[E1, E2, E],
                 ev      : E ContainsSubTypesOf Element
      ) =
        new Append[Sequence[E1], Sequence[E2]] {
          type Elements = E
          def apply(a:Sequence[E1], b: Sequence[E2]) = Sequence(prepend(a.elements, b.elements))
        }
    }

    abstract class Choose[A, B] {
      type Elements <: HList
      def apply(a:A, b:B): Choice[Elements]
    }

    trait LowerPriorityChoose0 {

      implicit def `e | e`[E1, E2](
        implicit ev: (E1 :: E2 :: HNil) ContainsSubTypesOf Element
      ) =
        new Choose[E1, E2] {
          type Elements = E1 :: E2 :: HNil
          def apply(a:E1, b:E2) = Choice(a :: b :: HNil)
        }
    }

    trait LowerPriorityChoose1 extends LowerPriorityChoose0 {

      implicit def `e | c`[E1, E2 <: HList](
        implicit ev: (E1 :: E2) ContainsSubTypesOf Element
      ) =
        new Choose[E1, Choice[E2]] {
          type Elements = E1 :: E2
          def apply(a:E1, b:Choice[E2]) = Choice(a :: b.options)
        }

      implicit def `c | e`[E1 <: HList, E2, E <: HList](
        implicit prepend : Prepend.Aux[E1, E2 :: HNil, E],
                 ev      : E ContainsSubTypesOf Element
      ) =
        new Choose[Choice[E1], E2] {
          type Elements = E
          def apply(a:Choice[E1], b:E2) = Choice(prepend(a.options, b :: HNil))
        }
    }

    object Choose extends LowerPriorityChoose1 {

      implicit def `c | c`[E1 <: HList, E2 <: HList, E <: HList](
        implicit prepend : Prepend.Aux[E1, E2, E],
                 ev      : E ContainsSubTypesOf Element
      ) =
        new Choose[Choice[E1], Choice[E2]] {
          type Elements = E
          def apply(a:Choice[E1], b:Choice[E2]) = Choice(prepend(a.options, b.options))
        }
    }
  }
}
