package qirx.parser
package details

import psp.api.View
import psp.std._
import qirx.parser.Failure
import qirx.parser.Input
import qirx.parser.Result

trait Constructors {
  def failure[A](failure:Failure): Failure | View[Result[A]] = Failed(failure)

  def success[A](value: A, remaining: Input): Failure | View[Result[A]] =
    Succeeded(Direct(Result(value, remaining)))

  def newView[A](a: A *): View[A] = Direct(a : _*).force
}
