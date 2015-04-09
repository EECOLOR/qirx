package utils

import org.qirx.littlespec.assertion.Assertion
import org.qirx.littlespec.Specification
import psp.api._
import psp.std._
import qirx.parser.details.ParseResult
import qirx.parser.details.Succeeded
import qirx.parser.details.Failed
import qirx.parser.Failure
import qirx.parser.Input
import qirx.parser.Result

trait CustomMatchers { _: Specification =>

  def beResult[A](expected: (A -> (Int, Input))*)(implicit force: Enforcer[A]): Assertion[ParseResult[Failure, View[Result[A]]]] =
    new Assertion[ParseResult[Failure, View[Result[A]]]] {
      def assert(result: => ParseResult[Failure, View[Result[A]]]) =
        result match {
          case Failed(failed) => scala.Left("Expected success, got: " + failed)
          case Succeeded(results) =>
            results.force.size is Size(expected.size) withMessage ("size is not expected: " + _)

            results zip expected foreach { case (result, expected) =>

                val (eValue, (ePosition, Input(eRemaining, 0)))  = expected
                val Result(rValue, Input(rRemaining, rPosition)) = result

                force(rValue)    is force(eValue)    withMessage ("result is incorrect: " + _)
                rRemaining.force is eRemaining.force withMessage ("remaining is incorrect: " + _)
                rPosition        is ePosition        withMessage ("position is incorrect: " + _)
            }

            scala.Right(success)
        }
    }

  def beFailure[T <: Failure : CTag](at: Long, input: Input): Assertion[ParseResult[Failure, _]] =
    new Assertion[ParseResult[Failure, _]] {
      def assert(result: => ParseResult[Failure, _]) = {
        result match {
          case Failed(failed) =>
            failed must beAnInstanceOf[T]

            val Input(fInput, fPosition) = failed.input
            val Input(eInput, 0) = input

            fInput.force is eInput.force withMessage ("input is incorrect: " + _)
            fPosition    is at           withMessage ("position is incorrect: " + _)
            scala.Right(success)
          case Succeeded(results) => scala.Left("Expected failure, got: " + results)
        }
      }
    }

  def be[A]: Assertion[A] = new Assertion[A] {
    def assert(s: => A) = scala.Right(success)
  }
}
