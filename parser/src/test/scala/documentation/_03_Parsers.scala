package documentation

import psp.api._
import psp.std.{Failure => _, _}
import psp.std.HashEq.universalEq
import psp.std.StdEq._
import qirx.parser._
import qirx.parser.parsers._
import qirx.parser.details.Consumed
import qirx.parser.details.Outcome
import qirx.parser.details.Rejected
import qirx.parser.details.SplitInput
import shapeless.HNil
import shapeless.::
import shapeless.test.illTyped
import utils.Documentation

object _03_Parsers extends Documentation {

"""|#Parsers
   |
   |We choose to create new parsers in order to have full control over over the readability
   |of the code.
   |
   |These parsers can handle ambiguity which allows us to use them for grammar checking.
   |
   |The parsers correspond do the elements in grammer:
   |
   |- The character parser can be used to parse `Terminal` elements
   |- The choice parser can be used to parse `Choice` elements
   |- The sequence parser can be used to parse `Sequence` (or `Nonterminal`) elements
   |- The not parser can be used to parse `Attributed.not` elements
   |- The zero or one parser can be used to parse `Attributed.zeroOrOne` elements
   |- The zero or more parser can be used to parse `Attributed.zeroOrMore` elements
   |- The one or more parser can be used to parse `Attributed.zeroOrOne` elements
   | """.stripMargin - {

  """|A parser is a type with the following signature
     | """.stripMargin - sideEffectExample {
       import qirx.parser.details.{ParseResult => |}
       type ??? = Any
       val parser = new Parser[???] {
         def parse(input: Input): Failure | View[Result[???]] = ???
       }
     }

     case class CustomResult(content: String) extends NaturalHashEq
     var characterParser: CharacterParser[CustomResult] = null

     val expectedInputMessage = "Expected input, but no input was provided"

  """|## Character parser
     |
     |This is a parser that consumes characters and converts it to the specified result.
     |
     |Below a simple definition that consumes any 'a' or 'b' character and converts its
     |retult to a custom case class.
     | """.stripMargin - sideEffectExample {
       def customConsume(characters: Input): Outcome =
         characters
           .span(c => c == 'a' || c == 'b')
           .toOutcome(ifRejected = "Expected `a` or `b` characters")

       def customToValue(consumed: InvariantView[Char] with HasPreciseSize): CustomResult =
         CustomResult(consumed.force[String])

       characterParser =
         CharacterParser(
           consume = customConsume,
           toValue = customToValue
         )
     }

     "- It returns a failure on empty input" - {
       characterParser parse "" must
         beFailure[ExpectedInput](at = 0, input = "", message = expectedInputMessage)
     }
     "- It returns a failure if the parser did not consume anything" - {
       characterParser parse "c" must
         beFailure[InvalidInput](at = 0, input = "c", message = "Expected `a` or `b` characters")
     }
     "- When it consumes input it will return the value together with the unconsumed input\n " - {
       characterParser parse "bac" must beResult(CustomResult("ba") -> (2 -> "c"))
     }

     var `abc`: Parser[String] = null

  """|### String parser
     |
     |We have supplied a string parser that can be used to match an exact string.
     | """.stripMargin - sideEffectExample {
       `abc` = CharacterParser.string("abc", identity)
     }

     "- It fails on input that does not start with the specified string" - {
       `abc` parse "dabc" must
         beFailure[InvalidInput](at = 0, input = "dabc", message = "Expected `abc`, got `dab`")
     }
     "- It returns the string with no remaining input if it consumed all characters" - {
       `abc` parse "abc" must beResult("abc" -> (3 -> ""))
     }
     "- It returns the correct remaining input if it did not consume all characters\n " - {
       `abc` parse "abcdefg" must beResult("abc" -> (3 -> "defg"))
     }

    val `abcd` = CharacterParser.string("abcd", identity)
    var choiceParser: Parser[String] = null

  """|## Choice parser
     |
     |This parser will try to parse input using all of the other parser.
     |
     |Below an example of a parser that possibly gives an ambiguous result.
     | """.stripMargin - sideEffectExample {
       choiceParser =
         ChoiceParser(
           parsers = Direct(`abc`, `abcd`),
           toValue = identity[String]
         )
     }
     "Note that this parser throws an exception if no parsers were given during construction" - {
       ChoiceParser(emptyValue[View[Parser[String]]], identity[String]) must throwA[Throwable]
     }
     "- It returns a failure if there none of the parsers match the input" - {
       choiceParser parse ""    must
         beFailure[ExpectedInput](at = 0, input = "", message = expectedInputMessage)

       choiceParser parse "def" must
         beFailure[InvalidInput] (at = 0, input = "def", message = "Expected `abc`, got `def`")
     }
     "- It returns the correct value if one of the parsers matched the input" - {
       choiceParser parse "abce" must beResult("abc" -> (3 -> "e"))
     }
     "- It returns multiple values if more than one matches the input" - {
       choiceParser parse "abcde" must beResult("abc" -> (3 -> "de"), "abcd" -> (4 -> "e"))
     }
     "- It correctly handles nested choice parsers\n " - {
       val c = ChoiceParser(Direct(choiceParser, choiceParser), identity[String])
       c parse "abcde" must beResult(
         "abc"  -> (3 -> "de"),
         "abcd" -> (4 -> "e" ),
         "abc"  -> (3 -> "de"),
         "abcd" -> (4 -> "e" )
       )
     }

     var sequenceParser: Parser[Result[String] :: Result[String] :: HNil] = null

  """|## Sequence parser
     |
     |This parser consumes the input using the given sequence of other parsers. It will then
     |convert the result to the specified result.
     |
     |Below a parser that is set up for ambiguity.
     | """.stripMargin - sideEffectExample {
       def takeExactly3(input: Input): Outcome = {
         val SplitInput(consumed, remaining) = input.splitAt(Precise(3))
         if (consumed.size == Size(3)) Consumed(consumed.underlying, remaining)
         else Rejected(s"Expected exactly 3 characters, got ${consumed.size}")
       }

       val take3 = CharacterParser(takeExactly3, _.force[String])

       val choice = ChoiceParser(Direct(`abc`, take3), identity[String])

       sequenceParser =
         SequenceParser(
           parsers = choice :: `abcd` :: HNil,
           toValue = identity[Result[String] :: Result[String] :: HNil]
         )
     }

     "Note that this parser will not compile if no parsers were given during construction" - {
       illTyped("SequenceParser(HNil:HNil, identity[HNil]) must throwA[Throwable]")
       success
     }

     def resultToTuple[A]: Result[A] => (A, Position, String) = result =>
       (result.value, result.position, result.remaining.underlying.force)

     def hlistResultToTuples[A]:Result[A] :: Result[A] :: HNil => (A, Position, String) :: (A, Position, String) :: HNil =
       hlist => resultToTuple(hlist.head) :: resultToTuple(hlist.tail.head) :: HNil

     "- It returns a failure when any of the given parsers returns a failure" - {
       sequenceParser parse "" must
         beFailure[ExpectedInput](at = 0, input = "", message = expectedInputMessage)
       sequenceParser parse "d" must
         beFailure[InvalidInput ](at = 0, input = "d", message = "Expected `abc`, got `d`")
       sequenceParser parse "abc" must
         beFailure[ExpectedInput](at = 3, input = "", message = expectedInputMessage)
       sequenceParser parse "abcd" must
         beFailure[InvalidInput ](at = 3, input = "d", message = "Expected `abcd`, got `d`")
     }
     "- It returns a result when all of the given parsers return a result" - {
       val result1 = sequenceParser parse "abcabcd" mapValue hlistResultToTuples
       result1 must beResult(
         (("abc", Position(0, 3), "abcd") :: ("abcd", Position(3, 7), "") :: HNil) -> (7 -> ""),
         (("abc", Position(0, 3), "abcd") :: ("abcd", Position(3, 7), "") :: HNil) ->(7 -> "")
       )
       val result2 = sequenceParser parse "123abcde" mapValue hlistResultToTuples
       result2 must beResult((("123", Position(0, 3), "abcde") :: ("abcd", Position(3, 7), "e") :: HNil) -> (7 -> "e"))
     }
     "- It correctly passes the remaining characters if they have multiple results for some combination\n " - {
       val parser = SequenceParser(
         parsers = choiceParser :: choiceParser :: HNil,
         toValue = hlistResultToTuples[String]
       )
       parser parse "abcabcd"  must beResult(
         ( ("abc", Position(0, 3), "abcd") :: ("abc",  Position(3, 6), "d") :: HNil) -> (6 -> "d"),
         ( ("abc", Position(0, 3), "abcd") :: ("abcd", Position(3, 7), "" ) :: HNil) -> (7 -> "")
       )
       parser parse "abcdabcd" must beResult(
         (("abcd", Position(0, 4), "abcd") :: ("abc",  Position(4, 7), "d") :: HNil) -> (7 -> "d"),
         (("abcd", Position(0, 4), "abcd") :: ("abcd", Position(4, 8), "" ) :: HNil) -> (8 -> "")
       )
       parser parse "abcdabc" must beResult((("abcd", Position(0, 4), "abc") :: ("abc", Position(4, 7), "") :: HNil) -> (7 -> ""))
       parser parse "abcabc"  must beResult((( "abc", Position(0, 3), "abc") :: ("abc", Position(3, 6), "") :: HNil) -> (6 -> ""))
     }

     var notParser: Parser[Char] = null

  """|## Not parser
     |
     |This consumes input that the underlying parser did not accept. Note that it completely
     |ignores the successful result of the underlying parser.
     |
     |Below a parser that consumes anything but the `x` character.
     |
     |Warning: this parser always succeeds, do not use it on it's own within a looping construct
     | """.stripMargin - sideEffectExample {
       notParser =
         NotParser(
           underlying = CharacterParser.string("x", identity),
           toValue    = identity[Char]
         )
     }

     def viewToString: View[Char] => String = _.force

     "- It will return a failure if the input is empty" - {
       notParser parse "" must
         beFailure[ExpectedInput](at = 0, input = "", message = expectedInputMessage)
     }
     "- It will not consume anything if the underlying parser consumed something" - {
       notParser parse "x" must
         beFailure[InvalidInput ](at = 0, input = "x", message = "Expected underlying parser to reject `x`, it consumed it")
     }
     "- It consumes if the underlying parser fails to do so" - {
       notParser parse "y" must beResult('y' -> (1 -> ""))
     }
     "- It returns the correct remaining characters" - {
       notParser parse "yy" must beResult('y' -> (1 -> "y"))
     }

     var zeroOrOneParser: Parser[Option[String]] = null

  """|## Zero or one parser
     |
     |This parser tries the underlying parser on the input
     |
     |Below a parser that tries the underlying parser, but will not fail if it fails.
     | """.stripMargin - sideEffectExample {
       zeroOrOneParser =
         ZeroOrOneParser(
           underlying = choiceParser,
           toValue    = identity[Option[String]]
         )
     }
     "- It will not return an error if no input is available" - {
       val expected: Option[String] = None
       zeroOrOneParser parse "" must beResult(expected -> (0 -> ""))
     }
     "- It will return the correct remaining characters on a mismatch" - {
       val expected: Option[String] = None
       zeroOrOneParser parse "1" must beResult(expected -> (0 -> "1"))
     }
     "- It returns the results of the parser on a match\n " - {
       zeroOrOneParser parse "abcd" must beResult(Option("abc") -> (3 -> "d"), Option("abcd") -> (4 -> ""))
     }

     var zeroOrMoreParser: Parser[View[Result[String]]] = null
     def viewResultToTuple[A]: View[Result[A]] => View[(A, Position, String)] =
       _.map(resultToTuple)

  """|## Zero or more parser
     |
     |This repeats the underlying parser zero or more times.
     |
     |Below a parser that never fails and executes the underlying parser as often as it can.
     | """.stripMargin - sideEffectExample {
       zeroOrMoreParser =
         ZeroOrMoreParser(
           underlying = choiceParser,
           toValue    = identity[View[Result[String]]]
         )
     }
     "- It will not return an error if no input is available" - {
       zeroOrMoreParser parse "" mapValue viewResultToTuple must
         beResult(newView[(String, Position, String)]() -> (0 -> ""))
     }
     "- It returns all remaining input if the underlying parser failed" - {
       zeroOrMoreParser parse "a" mapValue viewResultToTuple must
         beResult(newView[(String, Position, String)]() -> (0 -> "a"))
       zeroOrMoreParser parse "abcde" mapValue viewResultToTuple must
         beResult(
           newView(("abc",  Position(0, 3), "de")) -> (3 -> "de"),
           newView(("abcd", Position(0, 4), "e" )) -> (4 -> "e")
         )
     }
     "- It executes the parser multiple times and be as greedy as it can be\n " - {
       zeroOrMoreParser parse "abcdabcd" mapValue viewResultToTuple must
         beResult(
           // This result is discarded: newView("abc") -> (3 -> "dabcd")
           newView(("abcd", Position(0, 4), "abcd"), ("abc",  Position(4, 7), "d")) -> (7 -> "d"),
           newView(("abcd", Position(0, 4), "abcd"), ("abcd", Position(4, 8), ""))  -> (8 -> "" )
         )
     }

     var oneOrMoreParser: Parser[View[Result[String]]] = null

  """|## One or more parser
     |
     |Repeats the underlying parser at least one time
     |
     |This parser that consumes the underlying input at least once
     | """.stripMargin - sideEffectExample {
       oneOrMoreParser =
         OneOrMoreParser(
           underlying = choiceParser,
           toValue    = identity[View[Result[String]]]
         )
     }
     "- It should return a failure when presented with no input" - {
       oneOrMoreParser parse "" mapValue viewResultToTuple must
         beFailure[ExpectedInput](at = 0, input = "", message = expectedInputMessage)
     }
     "- It should report a failure when the underlying parser fails" - {
       oneOrMoreParser parse "a" mapValue viewResultToTuple must
         beFailure[InvalidInput](at = 0, input = "a", message = "Expected `abc`, got `a`")
     }
     "- It succeeds if the underlying parser consumed at least once" - {
       oneOrMoreParser parse  "abc" mapValue viewResultToTuple must
         beResult(newView(("abc", Position(0, 3), "" )) -> (3 -> "" ))
       oneOrMoreParser parse "abce" mapValue viewResultToTuple must
         beResult(newView(("abc", Position(0, 3), "e")) -> (3 -> "e"))
       oneOrMoreParser parse "abcde" mapValue viewResultToTuple must
         beResult(
           newView(("abc",  Position(0, 3), "de")) -> (3 -> "de"),
           newView(("abcd", Position(0, 4), "e" )) -> (4 -> "e")
         )
     }
     "- It succeeds if the underlying parser can consume multiple times\n " - {
       oneOrMoreParser parse "abcdabcd" mapValue viewResultToTuple must
         beResult(
           // This result is discarded: newView("abc") -> (3 -> "dabcd")
           newView(("abcd", Position(0, 4), "abcd"), ("abc",  Position(4, 7), "d")) -> (7 -> "d"),
           newView(("abcd", Position(0, 4), "abcd"), ("abcd", Position(4, 8), "" )) -> (8 -> "" )
         )
     }
   }
}
