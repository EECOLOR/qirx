package qirx.parser
package grammar
package details

import psp.api.View
import psp.std._
import shapeless.Generic
import shapeless.HList
import shapeless.HNil
import shapeless.::
import qirx.parser.Result
import qirx.parser.Position

trait Constructor[-A, B] extends (Result[A] => B) {
  def apply(a:Result[A]):B
}

trait LowerPriorityConstructor {
  import Constructor.exact
  import Constructor.utilities._

  implicit def hlistPattern[A <: HList, B <: HList, C](
    implicit simplified  : A SimplifiedAs B,
             alignedTo   : B AlignedTo (C :: HNil),
             addPosition : AddPositionsTo[C]
  ) =
    new Constructor[A, C] {
      def apply(result: Result[A]) =
        result.map(_ |> simplified |> (_.head)) |> exact
    }
}

object Constructor extends LowerPriorityConstructor {

  import utilities._

  implicit def exact[A](
    implicit addPosition : AddPositionsTo[A]
  ) =
    new Constructor[A, A] {
      def apply(result: Result[A]) = {
        val instance = result.value
        addPosition(instance, result.position)
        instance
      }
    }

  implicit def view[A, B](
    implicit constructor: Constructor[A, B]
  ) =
    new Constructor[View[Result[A]], View[B]] {
      def apply(result: Result[View[Result[A]]]) =
        result.value map constructor
    }

  implicit def viewAsString[A](
    implicit constructor: Constructor[String, A]
  ) =
    new Constructor[View[Result[Char]], A] {
      def apply(result: Result[View[Result[Char]]]) =
        result.map(_.map(_.value).force) |> constructor
    }

  implicit def caseClassLike[A, C <: HList, D <: HList, E <: HList, B](
    implicit construct   : Generic.Aux[B, E],
             toHList     : A ToHList C,
             simplified  : C SimplifiedAs D,
             toArguments : D AlignedTo E,
             addPosition : AddPositionsTo[B]
  ): Constructor[A, B] =
      new Constructor[A, B] {
        def apply(result: Result[A]) = {
          val arguments = result |> toHList |> simplified |> toArguments
          val instance = construct from arguments
          addPosition(instance, result.position)
          instance
        }
      }

  object utilities {

    // Utility traits are marked as sealed because we do not want them to be implemented by the
    // public. They are an implementation detail.

    sealed trait AddPositionsTo[T] {
      def apply(instance:T, position: Position): Unit
    }

    sealed trait LowerPriorityAddPositionsTo {

      implicit def any[T] =
        new AddPositionsTo[T] {
          def apply(instance:T, position: Position) = ()
        }
    }

    object AddPositionsTo extends LowerPriorityAddPositionsTo {

      implicit def positioned[T <: Positioned] =
        new AddPositionsTo[T] {
          def apply(instance: T, position: Position) = {
            if (instance.position == Position.None) {
              implicit val key: Positioned.Key = null
              instance.position = position
            }
          }
        }
    }

    sealed trait AlignedTo[-I <: HList, O <: HList] extends (I => O) {
      def apply(list : I): O
    }

    object AlignedTo {

      implicit def hnil =
        new (HNil AlignedTo HNil) {
          def apply(list: HNil) = list
        }

      implicit def hlist[H1, H2, T <: HList, O <: HList](
        implicit alignedHead : Constructor[H1, H2],
                 alignedTail : T AlignedTo O
       ) =
          new ((Result[H1] :: T) AlignedTo (H2 :: O)) {
            def apply(list: Result[H1] :: T) = alignedHead(list.head) :: alignedTail(list.tail)
          }
    }

    sealed trait SimplifiedAs[-I <: HList, O <: HList] extends (I => O) {
      def apply(list: I): O
    }

    sealed trait LowerPrioritySimplifiedAs {

      implicit def sameHead[H, T1 <: HList, T2 <: HList](
        implicit tail        : T1 SimplifiedAs T2
      ) =
        new ((H :: T1) SimplifiedAs (H :: T2)) {
          def apply(list: H :: T1) = list.head :: tail(list.tail)
        }
    }

    object SimplifiedAs extends LowerPrioritySimplifiedAs {

      implicit def hnil =
        new (HNil SimplifiedAs HNil) {
          def apply(list: HNil) = list
        }

      implicit def viewPattern[H1, T1 <: HList, T2 <: HList, A](
        implicit simplifyTail: T1 SimplifiedAs T2
      ) =
        new ((Result[H1] :: Result[Results[H1]] :: T1) SimplifiedAs (Result[Results[H1]] :: T2)) {

          def apply(list: Result[H1] :: Result[Results[H1]] :: T1) = {

            val first  = list.head
            val second = list.tail.head
            val tail   = list.tail.tail

            val Result(results, secondPosition, remaining) = second

            val newPosition = Position(first.position.start, secondPosition.end)
            val newResult   = Result(first +: results, newPosition, remaining)

            newResult :: simplifyTail(tail)
          }
      }
    }

    sealed trait ToHList[-A, B <: HList] extends (Result[A] => B) {
      def apply(result: Result[A]):B
    }

    sealed trait LowerPriorityToHList {

      implicit def single[A] =
        new (A ToHList (Result[A] :: HNil)) {
          def apply(result: Result[A]) = result :: HNil
        }
    }

    object ToHList extends LowerPriorityToHList {

      implicit def hlist[T <: HList] =
        new (T ToHList T) {
          def apply(result: Result[T]) = result.value
        }
    }
  }
}
