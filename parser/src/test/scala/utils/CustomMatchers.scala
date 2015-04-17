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
import qirx.parser.Position
import qirx.parser.Result

trait CustomMatchers { _: Specification =>

  def beResult[A](expected: (A -> (Int, String))*)(implicit force: Enforcer[A]): Assertion[ParseResult[Failure, View[Result[A]]]] =
    new Assertion[ParseResult[Failure, View[Result[A]]]] {
      def assert(result: => ParseResult[Failure, View[Result[A]]]) =
        result match {
          case Failed(failed) => scala.Left("Expected success, got: " + failed)
          case Succeeded(results) =>
            results.force.size is Size(expected.size) withMessage ("size is not expected: " + _)

            results zip expected foreach { case (result, expected) =>

                val (eValue, (eEnd, eRemaining))  = expected
                val Result(rValue, rPosition, Input(rRemaining, rEnd)) = result

                force(rValue)      is force(eValue)      withMessage ("result is incorrect: " + _)
                rRemaining.force   is eRemaining.force   withMessage ("remaining is incorrect: " + _)
                rEnd               is eEnd               withMessage ("remaining position is incorrect: " + _)
                rPosition          is Position(0, eEnd)  withMessage ("position is incorrect: " + _)
            }

            scala.Right(success)
        }
    }

  def beFailure[T <: Failure : CTag](at: Long, input: String, message: String): Assertion[ParseResult[Failure, _]] =
    new Assertion[ParseResult[Failure, _]] {
      def assert(result: => ParseResult[Failure, _]) = {
        result match {
          case Failed(failed) =>
            failed must beAnInstanceOf[T]

            val Input(fInput, fPosition) = failed.input

            fInput.force   is input   withMessage ("input is incorrect: " + _)
            fPosition      is at      withMessage ("position is incorrect: " + _)
            failed.message is message withMessage ("message is incorrect: " + _)
            scala.Right(success)
          case Succeeded(results) => scala.Left("Expected failure, got: " + results)
        }
      }
    }

  def be[A]: Assertion[A] = new Assertion[A] {
    def assert(s: => A) = scala.Right(success)
  }
}
