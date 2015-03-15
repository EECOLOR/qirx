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

trait Documentation extends Specification {
  /* required for the location macro of little-spec */
  protected[this] val Seq = psp.std.scSeq

  def sideEffectExample(code: => Unit)(implicit location: Location): Fragment =
    createFragment(Source.codeAtLocation(location), { code; success })

  def beResult[A](expected: (A -> View[Char] with HasPreciseSize)*) =
    new Assertion[Either[Failure, View[Result[A]]]] {
      def assert(result: => Either[Failure, View[Result[A]]]) =
        result match {
          case Left(failed) => scala.Left("Expected success, got: " + failed)
          case Right(results) =>
            results.force.size is Size(expected.size)
            results zip expected foreach { case (result, expected) =>
              val (value, remaining) = expected
              result.value is value
              result.remaining.force is remaining.force
            }
            scala.Right(success)
        }
    }
}
