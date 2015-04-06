package qirx.parser
package details

import psp.api.View
import psp.std._
import qirx.parser.Failure
import qirx.parser.Result

trait Enhancements {

  implicit class ParseResultEnhancement[A](result: Failure | View[Result[A]]) {

    def mapValue[B](f: A => B): Failure | View[Result[B]] =
      result map (_ map (_ map f))

    def flatMapResult[B](f: Result[A] => Failure | View[Result[B]]): Failure | View[Result[B]] = {

      def toResultView: View[Result[A]] => View[Failure | Result[B]] =
        _.flatMap(f andThen (_.toResultView))

      result.flatMap(toResultView andThen (_.toParseResult))
    }

    def toResultView: View[Failure | Result[A]] =
      result.fold(
        ifLeft  = failure => newView(Failed(failure)),
        ifRight = _ map Succeeded.apply
      )

    def isSuccess: Boolean = result.fold(_ => false, _ => true)

    def repeatWith[B](parser: Parser[B])(implicit ev: A => View[B]): Failure | View[Result[View[B]]] = {
      var lastResult = result mapValue ev
      var continue = result.isSuccess

      while (continue) {
        val parseResult =
          lastResult flatMapResult {
            case Result(values, remaining) => parser parse remaining mapValue (values :+ _)
          }
        continue = parseResult.isSuccess
        if (continue) {
          lastResult = parseResult
        }
      }

      lastResult
    }
  }

  implicit class ParseResultViewEnhancements[A](view: View[Failure | Result[A]]) {

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
