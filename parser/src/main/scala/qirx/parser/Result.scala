package qirx.parser

import psp.api._
import psp.api.Api._

case class Result[A](value: A, remaining: InvariantView[Char]) {
  def map[B](f: A => B): Result[B] = Result(f(value), remaining)
}
