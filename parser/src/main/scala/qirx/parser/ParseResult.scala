package qirx.parser

import psp.api._
import psp.std.{Failure => _, _}
import org.qirx.programbuilder._

case class ParseResult[A](result: Failure | A) {

  def transform[B, C](f: B => C)(implicit ev: A => View[Result[B]]):Failure | View[Result[C]] =
    result.map(ev andThen (_.map(_.map(f))))

  def isSuccess = result.fold(_ => false, _ => true)
}

object ParseResult {

  def withSingleResult[A](value: A, input: InvariantView[Char]): ParseResult[View[Result[A]]] =
    ParseResult(Right(Direct(Result(value, input))))

  /* The problem that some parsers presented was quite tricky. Each parser of the parsers that
   * are given can return multiple results. All consecutive parsers needed to produce a result
   * for each of the previous results.
   *
   * My intuition told me that I should write it down in a for comprehension. All we needed
   * to do was extract results and eventually combine them. The problem was that we are
   * dealing with both `Either` and `View` instances (not to mention the nested `Result` and
   * `View` types). The for comprehension does not deal well with different 'container' types.
   *
   * The traditional way to approach this is to use monad transformers. And they simply mess up
   * my mind. So I used the alternative approach that allows me to cut up the problem in smaller
   * steps.
   *
   * Every part of the for comprehension now results in a program that can contain multiple types
   * of instructions. In this case a `View` and `ParseResult` instruction. In the `runner` I can
   * transform those types into a common type I called `ViewResult`.
   *
   * In order to run the program I need a `Monadic` for that type (the actual implementation of
   * `flatMap`).
   *
   * Once I have gathered all possible results I need to put them into two piles: failures and
   * successes. If there are no successes, we will use the first failure. Otherwise we pass on
   * the successes to the next parser.
   *
   * In conclusion, this approach allowed me to separate construction, implementation and
   * execution and made it possible for me to solve this complex problem in small steps. I hope
   * they are small enough for me, and anyone else to understand.
   */

  private type ParseResultType[A] = ParseResult[View[Result[View[A]]]]

  private implicit val programType = ProgramType[View :+: ParseResult :+: CNil]

  def parseAndConcatenate[A]: (ParseResultType[A], Parser[A]) => ParseResultType[A] = {
    (previousParseResult, parser) =>

      val program =
        for {
          previousResults                  <- previousParseResult
          Result(previousValue, remaining) <- previousResults.toProgram
          newResults                       <- ParseResult(parser parse remaining)
          Result(newValue, newRemaining)   <- newResults.toProgram
        } yield Result(previousValue :+ newValue, newRemaining)

      val result = program runWith runner

      val (failures, successes) = split(result)

      if (successes.isEmpty) ParseResult(Left(failures.head))
      else ParseResult(Right(successes))
  }

  private type ViewResult[A] = View[Failure | A]

  private val runner = {
    val viewRunner = new (View ~> ViewResult) {
      def transform[x] = _.map(Right(_))
    }

    val parseResultRunner = new (ParseResult ~> ViewResult) {
      def transform[x] = p => Direct(p.result)
    }

    viewRunner :+: parseResultRunner
  }

  private implicit val ViewResultMonadic: Monadic[ViewResult] = new Monadic[ViewResult] {

    def create[A](a: A): View[Failure | A] = Direct(Right(a))

    def flatMap[A, B](fa: View[Failure | A])(f: A => View[Failure | B]) =
      fa.foldl(emptyValue[View[Failure | B]]) {
        case (previousResult, Left(failure)) => previousResult :+ Left(failure)
        case (previousResult, Right(a))      => previousResult ++ f(a)
      }
  }

  private def split[A](results: View[Failure | Result[A]]): (View[Failure], View[Result[A]]) =
    results.foldl(emptyValue[(View[Failure], View[Result[A]])]) {
      case ((failures, successes), Left(failure))  => (failures :+ failure, successes)
      case ((failures, successes), Right(success)) => (failures, successes :+ success)
    }
}

