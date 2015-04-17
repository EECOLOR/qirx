package qirx.parser
package details

import psp.std.String

sealed trait Outcome {
  def fold[A](ifConsumed: (Input, Input) => A, ifRejected: String => A): A =
    this match {
      case Consumed(consumed, remaining) => ifConsumed(consumed, remaining)
      case Rejected(message)             => ifRejected(message)
    }
}

case class Consumed(consumed: Input, remaining: Input) extends Outcome
case class Rejected(message: String) extends Outcome
