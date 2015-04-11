package qirx.parser
package details

import psp.std.Nothing

sealed trait ParseResult[+A, +B] {
  def map[C](f: B => C): A | C = this match {
    case Failed(value)    => Failed(value)
    case Succeeded(value) => Succeeded(f(value))
  }
  def flatMap[C, AA >: A](f: B => AA | C): AA | C = this match {
    case Failed(value)    => Failed(value)
    case Succeeded(value) => f(value)
  }
  def fold[C](ifFailure: A => C, ifSuccess: B => C): C = this match {
    case Failed(value)    => ifFailure(value)
    case Succeeded(value) => ifSuccess(value)
  }
}

object ParseResult extends ParseResultOperations

case class Failed[+A]   (value: A) extends ParseResult[A, Nothing]
case class Succeeded[+A](value: A) extends ParseResult[Nothing, A]
