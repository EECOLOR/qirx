package qirx.parser

import psp.api._
import psp.std._

trait Constructors {
  def failure[A](failure:Failure): Failure | View[Result[A]] = Left(failure)

  def success[A](value: A, remaining: View[Char]): Failure | View[Result[A]] =
    Right(Direct(Result(value, remaining.force)))

  def newView[A](a: A *): View[A] = Direct(a : _*).force
}
