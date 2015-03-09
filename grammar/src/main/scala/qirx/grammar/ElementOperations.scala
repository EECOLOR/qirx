package qirx.grammar

import psp.api._
import psp.std._

trait ElementOperations  {
  implicit class ElementOperations[A <: Element](a:A) {
    def ~ [B <: Element](b: B)(implicit append:A ~ B):Sequence = append(a, b)
    def | [B <: Element](b: B)(implicit choose:A | B):Choice   = choose(a, b)

    def       + : AttributedElement = AttributedElement(a)(oneOrMore  = true)
    def       * : AttributedElement = AttributedElement(a)(zeroOrMore = true)
    def       ? : AttributedElement = AttributedElement(a)(zeroOrOne  = true)
    def unary_! : AttributedElement = AttributedElement(a)(not        = true)
  }

  final class ~[A, B](f: (A, B) => Direct[Element]) {
    def apply(a:A, b:B): Sequence = Sequence(f(a, b))
  }
  object ~ {
    type E = Element
    implicit def `e ~ e`[E1 <: E, E2 <: E]: E1 ~ E2       = new (      E1 ~ E2      )(Direct.empty :+ (_:E) :+ (_:E))
    implicit def `e ~ s`[E1 <: E]:          E1 ~ Sequence = new (      E1 ~ Sequence)(_ +: _.elements)
    implicit def `s ~ e`[E2 <: E]:    Sequence ~ E2       = new (Sequence ~ E2      )(_.elements :+ _)
    implicit def `s ~ s`:             Sequence ~ Sequence = new (Sequence ~ Sequence)(_.elements ++ _.elements)
  }

  final class |[A, B](f: (A, B) => Direct[Element]) {
    def apply(a:A, b:B): Choice = Choice(f(a, b))
  }
  object | {
    type E = Element
    implicit def `e | e`[E1 <: E, E2 <: E]: E1 | E2     = new (    E1 | E2    )(Direct.empty :+ (_:E) :+ (_:E))
    implicit def `e | s`[E1 <: E]:          E1 | Choice = new (    E1 | Choice)(_ +: _.options)
    implicit def `s | e`[E2 <: E]:      Choice | E2     = new (Choice | E2    )(_.options :+ _)
    implicit def `s | s`:               Choice | Choice = new (Choice | Choice)(_.options ++ _.options)
  }
}
