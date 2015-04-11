package qirx.parser
package details

import psp.api.View
import psp.std._
import qirx.parser.Failure
import qirx.parser.Parser
import qirx.parser.Result

trait ParseResultOperations {

  implicit class `Operations for Failure | View[Result[A]]`[A](
    result: Failure | View[Result[A]]
  ) {

    def mapValue[B](f: A => B): Failure | View[Result[B]] =
      result map (_ map (_ map f))

    def mapResult[B](f: Result[A] => Result[B]): Failure | View[Result[B]] =
      result map (_ map f)

    def flatMapResult[B](f: Result[A] => Failure | View[Result[B]]): Failure | View[Result[B]] = {

      def toResultView: View[Result[A]] => View[Failure | Result[B]] =
        _.flatMap(f andThen (_.toResultView))

      result.flatMap(toResultView andThen (_.toParseResult))
    }

    def toResultView: View[Failure | Result[A]] =
      result.fold(
        ifFailure = failure => newView(Failed(failure)),
        ifSuccess = _ map Succeeded.apply
      )

    def isSuccess: Boolean = result.fold(_ => false, _ => true)
  }

  implicit class `Operations for Results[View[Result[A]]`[A](
    result: Failure | Results[View[Result[A]]]
  ) {

    def repeatWith(parser: Parser[A]): Failure | Results[View[Result[A]]] = {
      var lastResult = result
      var continue = result.isSuccess

      while (continue) {
        val parseResult = lastResult continueWith parser
        continue = parseResult.isSuccess
        if (continue) {
          lastResult = parseResult
        }
      }

      lastResult
    }

    def continueWith(parser: Parser[A]): Failure | Results[View[Result[A]]] =
      result flatMapResult {
        case Result(values, position, remaining) =>
          parser parse remaining mapResult {
            case result @ Result(_, newPosition, newRemaining) =>
              Result(values :+ result, Position(position.start, newPosition.end), newRemaining)
          }
      }
  }

  implicit class `Operations for View[Failure | Result[A]]`[A](
    view: View[Failure | Result[A]]
  ) {

    def split: (View[Failure], View[Result[A]]) =
      view.foldl(emptyValue[(View[Failure], View[Result[A]])]) {
        case ((failures, successes),    Failed(failure)) => (failures :+ failure, successes)
        case ((failures, successes), Succeeded(success)) => (failures, successes :+ success)
      }

    def toParseResult: Failure | View[Result[A]] = {
      val (failures, successes) = view.split

      if (successes.isEmpty) Failed(failures.head)
      else Succeeded(successes)
    }
  }
}
