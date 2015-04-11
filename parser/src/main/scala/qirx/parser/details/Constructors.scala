package qirx.parser
package details

import psp.api.InvariantView
import psp.api.HasPreciseSize
import psp.api.View
import psp.std._
import qirx.parser.Failure
import qirx.parser.Result

trait Constructors {

  def failure[A](failure:Failure): Failure | View[Result[A]] = Failed(failure)

  def emptySuccess[A](value: A, input:Input): Failure | View[Result[A]] =
    Succeeded(Direct(emptyResult(value, input)))

  def success[A](result: SplitInput, toValue: InvariantView[Char] with HasPreciseSize => A): Failure | View[Result[A]] = {
    val SplitInput(consumed, remaining) = result
    Succeeded(Direct(Result(toValue(consumed.underlying), Position(consumed.position, remaining.position), remaining)))
  }

  def emptyResult[A](value: A, input: Input): Result[A] =
    Result(value, Position(input.position, input.position), input)

  def newView[A](a: A *): View[A] = Direct(a : _*).force
}
