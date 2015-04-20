package qirx.parser
package details

import psp.api._
import psp.std._

sealed trait Outcome {
  def fold[A](
    ifConsumed: (InvariantView[Char] with HasPreciseSize, Input) => A,
    ifRejected: String => A
  ): A =
    this match {
      case Consumed(consumed, remaining) => ifConsumed(consumed, remaining)
      case Rejected(message)             => ifRejected(message)
    }
}

case class Consumed(consumed: InvariantView[Char] with HasPreciseSize, remaining: Input) extends Outcome
case class Rejected(message: String) extends Outcome
