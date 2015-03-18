package documentation

import org.qirx.littlespec.assertion.Assertion
import org.qirx.littlespec.fragments.Fragment
import org.qirx.littlespec.io.Source
import org.qirx.littlespec.macros.Location
import org.qirx.littlespec.Specification
import psp.api._
import psp.std.{ Failure => _, _ }
import qirx.parser.Failure
import qirx.parser.Result
import qirx.parser.Either
import qirx.parser.Left
import qirx.parser.Right
import qirx.parser.Input

trait Documentation extends Specification {
  /* required for the location macro of little-spec */
  protected[this] val Seq = psp.std.scSeq

  def sideEffectExample(code: => Unit)(implicit location: Location): Fragment =
    createFragment(Source.codeAtLocation(location), { code; success })

  def beResult[A](expected: (A -> (Int, Input))*)(implicit force: Enforcer[A]) =
    new Assertion[Either[Failure, View[Result[A]]]] {
      def assert(result: => Either[Failure, View[Result[A]]]) =
        result match {
          case Left(failed) => scala.Left("Expected success, got: " + failed)
          case Right(results) =>
            results.force.size is Size(expected.size) withMessage ("size is not expected: " + _)

            results zip expected foreach { case (result, expected) =>
                val (value, (position, remaining)) = expected
                force(result.value) is force(value) withMessage ("result is incorrect: " + _)
                result.remaining.underlying.force is remaining.underlying.force withMessage("remaining is incorrect: " + _)
                result.remaining.position is position withMessage ("position is incorrect: " + _)
            }
            scala.Right(success)
        }
    }

  trait Enforcer[A] {
    def apply(a:A):A
  }
  trait LowerPriorityEnforcers {
    implicit def any[A]:Enforcer[A] = new Enforcer[A] {
      def apply(a:A):A = a
    }
  }
  object Enforcer extends LowerPriorityEnforcers {
    implicit def view[A](implicit force: Enforcer[A]):Enforcer[View[A]] = new Enforcer[View[A]] {
      def apply(a:View[A]):View[A] = (a map force.apply).force
    }
  }
}
