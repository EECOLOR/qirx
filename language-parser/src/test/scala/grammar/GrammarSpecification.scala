package qirx.language

import psp.api._
import psp.std._
import psp.std.StdEq._
import org.qirx.littlespec.Specification
import qirx.parser.Result
import qirx.parser.Failure
import qirx.parser.|

trait GrammarSpecification extends Specification {
  implicit class ParseResultChecker[A](result: Failure | View[Result[A]]) {

    def resultsIn(expected: A) = {
      def checkResult(results: View[Result[A]]) = {
        val result = results.filter(_.remaining.isEmpty).force
        if (result.size === 1.size) {
          val Result(value, _, remaining) = result.head
          value is expected
          remaining.isEmpty is true withMessage "Expected all input to be consumed"
        }
        else {
          if (results.size === 1.size)
            failure(s"Got a single result, but some characters remained: ${results.head.remaining.underlying.force}")
          else failure(s"Expected a single result, got ${result.size}: ${result.force}")
        }
      }

      result.fold(
        ifFailure = _.toString |> failure,
        ifSuccess = checkResult
      )
    }
  }
}
