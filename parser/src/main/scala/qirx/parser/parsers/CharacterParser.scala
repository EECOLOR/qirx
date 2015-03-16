package qirx.parser
package parsers

import psp.api._
import psp.std.{ Failure => _, _ }

case class CharacterParser[A](
  consume: InvariantView[Char] => Split[Char] /* TODO: this should be SplitView, but that has no unapply */ ,
  toValue: InvariantView[Char] => A) extends Parser[A] {

  def parse(input: InvariantView[Char]): Failure | View[Result[A]] =
    if (input.isEmpty) failure(ExpectedInput)
    else {
      val Split(consumed, remaining) = consume(input)
      if (consumed.isEmpty) failure(InvalidInput)
      else success(toValue(consumed.force), remaining)
    }
}

object CharacterParser {

  def string(value: View[Char] with HasPreciseSize): CharacterParser[String] = {
    val size = value.size
    CharacterParser(
      consume = { input =>
        val consumed = input.take(size)
        if (consumed.force == value.force) Split(consumed, input.drop(size))
        else Split(emptyValue[View[Char]], input)
      },
      toValue = _.force
    )
  }

  def char(value: Char): CharacterParser[Char] = {
    CharacterParser(
      consume = _.span(_ == value),
      toValue = _.head
    )
  }
}
