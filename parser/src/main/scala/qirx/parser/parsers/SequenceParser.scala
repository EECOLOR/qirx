package qirx.parser
package parsers

import psp.api._
import psp.std.{ Failure => _, _ }
import org.qirx.programbuilder._

case class SequenceParser[A, B](
  parsers: View[Parser[A]],
  toValue: View[A] => B
) extends Parser[B] {

  if (parsers.isEmpty) abort("Can not operate without any parsers.")

  import SequenceParser._

  private type ParseResultType = ParseResult[View[Result[View[A]]]]

  def parse(input: InvariantView[Char]): Failure | View[Result[B]] = {

    val start = ParseResult.withSingleResult(emptyValue[View[A]], input)

    val ParseResult(result) = parsers.foldl(start)(parseAndConcatenate)

    // We need to apply the `toValue` method to all of the results, see `ParseResultType`
    // above to see why we need to go that deep to get at `View[A]`
    result.map(_.map(_.map(toValue)))
  }

  private val parseAndConcatenate: (ParseResultType, Parser[A]) => ParseResultType = {
    case (previousParseResult, parser) =>
      implicit val programType = ProgramType[View :+: ParseResult :+: CNil]

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
      // See SequenceParser companion object for more details on the chose approach
  }
}

object SequenceParser {
  /* The problem this parser presented was quite tricky. Each parser of the parsers that are
   * given can return multiple results. All consecutive parsers needed to produce a result
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

  // Ideally this would be a type alias, however we need to wrap the Either type because
  // type aliases are very fragile in the compiler
  private case class ParseResult[A](result: Failure | A)
  private object ParseResult {
    private[SequenceParser] def withSingleResult[A](value: A, input: InvariantView[Char]): ParseResult[View[Result[A]]] =
      ParseResult(Right(Direct(Result(value, input))))
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
