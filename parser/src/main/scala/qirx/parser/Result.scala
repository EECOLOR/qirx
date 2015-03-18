package qirx.parser

import psp.api._
import psp.std._

case class Result[A](value: A, remaining: Input) {
  def map[B](f: A => B): Result[B] = Result(f(value), remaining)
}
