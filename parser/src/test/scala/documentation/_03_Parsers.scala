package documentation

import psp.api._
import psp.std.{Failure => _, _}
import psp.std.HashEq.universalEq
import psp.std.StdEq._
import qirx.parser._
import qirx.parser.parsers._
import qirx.parser.details.SplitInput
import shapeless.::
import shapeless.HNil
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

  """|## Character parser
     |
     |This is a parser that consumes characters and converts it to the specified result.
     |
     |Below a simple definition that consumes any 'a' or 'b' character and converts its
     |retult to a custom case class.
     | """.stripMargin - sideEffectExample {
       def customConsume(characters: Input): SplitInput =
         characters.span(c => c == 'a' || c == 'b')

       def customToValue(consumed: InvariantView[Char] with HasPreciseSize): CustomResult =
         CustomResult(consumed.force[String])

       characterParser =
         CharacterParser(
           consume = customConsume,
           toValue = customToValue
         )
     }

     "- It returns a failure on empty input" - {
       characterParser parse "" must beFailure[ExpectedInput](at = 0, input = "")
     }
     "- It returns a failure if the parser did not consume anything" - {
       characterParser parse "c" must beFailure[InvalidInput](at = 0, input = "c")
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
       `abc` parse "dabc" must beFailure[InvalidInput](at = 0, input = "dabc")
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
       choiceParser parse ""    must beFailure[ExpectedInput](at = 0, input = "")
       choiceParser parse "def" must beFailure[InvalidInput] (at = 0, input = "def")
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

     var sequenceParser: Parser[String :: String :: HNil] = null

  """|## Sequence parser
     |
     |This parser consumes the input using the given sequence of other parsers. It will then
     |convert the result to the specified result.
     |
     |Below a parser that is set up for ambiguity.
     | """.stripMargin - sideEffectExample {
       def takeExactly3(input: Input): SplitInput = {
         val result @ SplitInput(consumed, _) = input.splitAt(Precise(3))
         if (consumed.size == Size(3)) result
         else SplitInput(Input.empty, input)
       }

       val take3 = CharacterParser(takeExactly3, _.mkString(""))

       val choice = ChoiceParser(Direct(`abc`, take3), identity[String])

       sequenceParser =
         SequenceParser(
           parsers = choice :: `abcd` :: HNil,
           toValue = identity[String :: String :: HNil]
         )
     }

     "Note that this parser will not compile if no parsers were given during construction" - {
       illTyped("SequenceParser(HNil:HNil, identity[HNil]) must throwA[Throwable]")
       success
     }
     "- It returns a failure when any of the given parsers returns a failure" - {
       sequenceParser parse ""     must beFailure[ExpectedInput](at = 0, input = "")
       sequenceParser parse "d"    must beFailure[InvalidInput ](at = 0, input = "d")
       sequenceParser parse "abc"  must beFailure[ExpectedInput](at = 3, input = "")
       sequenceParser parse "abcd" must beFailure[InvalidInput ](at = 3, input = "d")
     }
     "- It returns a result when all of the given parsers return a result" - {
       val result1 = sequenceParser parse "abcabcd"
       result1 must beResult(("abc" :: "abcd" :: HNil) -> (7 -> ""), ("abc" :: "abcd" :: HNil) ->(7 -> ""))
       val result2 = sequenceParser parse "123abcde"
       result2 must beResult(("123" :: "abcd" :: HNil) -> (7 -> "e"))
     }
     "- It correctly passes the remaining characters if they have multiple results for some combination\n " - {
       val parser = SequenceParser(
         parsers = choiceParser :: choiceParser :: HNil,
         toValue = identity[String :: String :: HNil]
       )
       parser parse "abcabcd"  must beResult(( "abc" :: "abc" :: HNil) -> (6 -> "d"), ( "abc" :: "abcd" :: HNil) -> (7 -> ""))
       parser parse "abcdabcd" must beResult(("abcd" :: "abc" :: HNil) -> (7 -> "d"), ("abcd" :: "abcd" :: HNil) -> (8 -> ""))
       parser parse "abcdabc"  must beResult(("abcd" :: "abc" :: HNil) -> (7 -> ""))
       parser parse "abcabc"   must beResult(( "abc" :: "abc" :: HNil) -> (6 -> ""))
     }

     var notParser: Parser[String] = null

  """|## Not parser
     |
     |This consumes input that the underlying parser did not accept. Note that it completely
     |ignores the successful result of the underlying parser.
     |
     |Below a parser that consumes anything but the `x` character.
     | """.stripMargin - sideEffectExample {
       notParser =
         NotParser(
           underlying = CharacterParser.string("x", identity),
           toValue    = _.force[String]
         )
     }
     "- It will return a failure if the input is empty" - {
       notParser parse "" must beFailure[ExpectedInput](at = 0, input = "")
     }
     "- It will not consume anything if the underlying parser consumed something" - {
       notParser parse "x" must beResult("" -> (0 -> "x"))
     }
     "- It consumes if the underlying parser fails to do so" - {
       notParser parse "yyy" must beResult("yyy" -> (3 -> ""))
     }
     "- It stops consuming as soon as the underlying parser starts to consume\n " - {
       notParser parse "yyyxxx" must beResult("yyy" -> (3 -> "xxx"))
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

     var zeroOrMoreParser: Parser[View[String]] = null

  """|## Zero or more parser
     |
     |This repeats the underlying parser zero or more times.
     |
     |Below a parser that never fails and executes the underlying parser as often as it can.
     | """.stripMargin - sideEffectExample {
       zeroOrMoreParser =
         ZeroOrMoreParser(
           underlying = choiceParser,
           toValue    = identity[View[String]]
         )
     }
     "- It will not return an error if no input is available" - {
       zeroOrMoreParser parse "" must beResult(newView[String]() -> (0 -> ""))
     }
     "- It returns all remaining input if the underlying parser failed" - {
       zeroOrMoreParser parse "a"     must beResult(newView[String]() -> (0 -> "a"))
       zeroOrMoreParser parse "abcde" must beResult(newView("abc") -> (3 -> "de"), newView("abcd") -> (4 -> "e"))
     }
     "- It executes the parser multiple times and be as greedy as it can be\n " - {
       zeroOrMoreParser parse "abcdabcd" must beResult(
         // This result is discarded: newView("abc") -> (3 -> "dabcd")
         newView("abcd", "abc")  -> (7 -> "d"),
         newView("abcd", "abcd") -> (8 -> "" )
       )
     }

     var oneOrMoreParser: Parser[View[String]] = null

  """|## One or more parser
     |
     |Repeats the underlying parser at least one time
     |
     |This parser that consumes the underlying input at least once
     | """.stripMargin - sideEffectExample {
       oneOrMoreParser =
         OneOrMoreParser(
           underlying = choiceParser,
           toValue    = identity[View[String]]
         )
     }
     "- It should return a failure when presented with no input" - {
       oneOrMoreParser parse "" must beFailure[ExpectedInput](at = 0, input = "")
     }
     "- It should report a failure when the underlying parser fails" - {
       oneOrMoreParser parse "a" must beFailure[InvalidInput](at = 0, input = "a")
     }
     "- It succeeds if the underlying parser consumed at least once" - {
       oneOrMoreParser parse  "abc"  must beResult(newView("abc") -> (3 -> "" ))
       oneOrMoreParser parse "abce"  must beResult(newView("abc") -> (3 -> "e"))
       oneOrMoreParser parse "abcde" must beResult(
         newView("abc") -> (3 -> "de"),
         newView("abcd") -> (4 -> "e")
       )
     }
     "- It succeeds if the underlying parser can consume multiple times\n " - {
       oneOrMoreParser parse "abcdabcd" must beResult(
         // This result is discarded: newView("abc") -> (3 -> "dabcd")
         newView("abcd", "abc")  -> (7 -> "d"),
         newView("abcd", "abcd") -> (8 -> "" )
       )
     }
   }
}
