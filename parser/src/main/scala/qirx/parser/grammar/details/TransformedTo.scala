package qirx.parser.grammar
package details

import psp.std._
import shapeless.HList
import shapeless.::

trait TransformedTo[-I, +O] { self =>
  def apply(i: I):O

  case class InContext[-T]() extends (I => O) {
    def apply(i: I): O = self(i)
  }
}

trait LowerPriorityDefaultTransformedTo {
  implicit def identity[I <: Element, T]: (I TransformedTo I)#InContext[T] =
    new (I TransformedTo I) {
      def apply(i: I):I = i
    }.InContext[T]
}

trait DefaultTransformedTo extends LowerPriorityDefaultTransformedTo {

  implicit def sequence[I <: HList, O <: HList, T](
    implicit transform : (I TransformedTo O)#InContext[T],
             ev        : O ContainsSubTypesOf Element
  ) =
    new (Sequence[I] TransformedTo Sequence[O]) {
      def apply(i: Sequence[I]) = Sequence(transform(i.elements))
    }.InContext[T]

  implicit def choice[I <: HList, O <: HList, T](
    implicit transform : (I TransformedTo O)#InContext[T],
             ev        : O ContainsSubTypesOf Element
  ) =
    new (Choice[I] TransformedTo Choice[O]) {
      def apply(i: Choice[I]) = Choice(transform(i.options))
    }.InContext[T]

  implicit def zeroOrOne[I <: Element, O <: Element, T](
    implicit transform : (I TransformedTo O)#InContext[T]
  ) =
    TransformedTo.forContext[T] { (i: ZeroOrOne[I]) =>
      ZeroOrOne(transform(i.element))
    }

  implicit def zeroOrMore[I <: Element, O <: Element, T](
    implicit transform : (I TransformedTo O)#InContext[T]
  ) =
    TransformedTo.forContext[T] { (i: ZeroOrMore[I]) =>
      ZeroOrMore(transform(i.element))
    }

  implicit def oneOrMore[I <: Element, O <: Element, T](
    implicit transform : (I TransformedTo O)#InContext[T]
  ) =
    TransformedTo.forContext[T] { (i: OneOrMore[I]) =>
      OneOrMore(transform(i.element))
    }

  implicit def not[I <: Element, O <: Element, T](
    implicit transform : (I TransformedTo O)#InContext[T]
  ) =
    TransformedTo.forContext[T] { (i: Not[I]) =>
      Not(transform(i.element))
    }
}

object TransformedTo extends DefaultTransformedTo {

  def forContext[X]: Factory[X] = new Factory[X]

  class Factory[X] {
    def apply[I, O](f: I => O): (I TransformedTo O)#InContext[X] =
      new (I TransformedTo O) {
        def apply(i: I) = f(i)
      }.InContext[X]
  }
}
