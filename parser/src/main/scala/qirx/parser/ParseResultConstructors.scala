package qirx.parser

import psp.api._
import psp.std._

trait ParseResultConstructors {
  def failure[A](failure:Failure): Failure | View[Result[A]] = Left(failure)

  def success[A](value: A, remaining: View[Char]): Failure | View[Result[A]] =
    Right(Direct(Result(value, remaining.force)))
}
