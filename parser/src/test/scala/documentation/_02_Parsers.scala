package documentation

import psp.api._
import psp.std.{Failure => _, _}
import psp.std.HashEq.universalEq
import psp.std.StdEq._
import qirx.parser._
import qirx.parser.parsers._

object _02_Parsers extends Documentation {

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
   |- The sequence parser can be used to parse `Sequence` elements
   |- the not parser can used to parse `Attributed.not` elements
   |- ???
   | """.stripMargin - {

     "[Note to self] Create property based tests for all parsers" - {}

  """|A parser is a type with the following signature
     | """.stripMargin - sideEffectExample {
       import qirx.parser.{Either => |}
       type ??? = Any
       val parser = new Parser[???] {
         def parse(input: InvariantView[Char]): Failure | View[Result[???]] = ???
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
       def customConsume(characters: InvariantView[Char]): SplitView[Char] =
         characters.span(c => c == 'a' || c == 'b')

       def customToValue(consumed: View[Char]): CustomResult =
         CustomResult(consumed.force[String])

       characterParser =
         CharacterParser(
           consume = customConsume,
           toValue = customToValue
         )
     }

     "- It returns a failure on empty input" - {
       characterParser parse "" is Left(ExpectedInput)
     }
     "- It returns a failure if the parser did not consume anything" - {
       characterParser parse "c" is Left(InvalidInput)
     }
     "- When it consumes input it will return the value together with the unconsumed input\n " - {
       characterParser parse "bac" must beResult(CustomResult("ba") -> "c")
     }

     var `abc`: Parser[String] = null

  """|### String parser
     |
     |We have supplied a string parser that can be used to match an exact string.
     | """.stripMargin - sideEffectExample {
       `abc` = CharacterParser.string("abc")
     }

     "- It fails on input that does not start with the specified string" - {
       `abc` parse "dabc" is Left(InvalidInput)
     }
     "- It returns the string with no remaining input if it consumed all characters" - {
       `abc` parse "abc" must beResult("abc" -> "")
     }
     "- It returns the correct remaining input if it did not consume all characters" - {
       `abc` parse "abcdef" must beResult("abc" -> "def")
     }

     var `a`: Parser[Char] = null // "

  """|### Single character parser
     |
     |We have supplied a character parser that can be used to match an exact string.
     | """.stripMargin - sideEffectExample {
       `a` = CharacterParser.char('a')
     }

     "- It fails on input that does not start with the specified character" - {
       `a` parse "b" is Left(InvalidInput)
     }
     "- It returns the character with no remaining input if it consumed all characters" - {
       `a` parse "a" must beResult('a' -> "")
     }
     "- It returns the correct remaining input if it did not consume all characters" - {
       `a` parse "abc" must beResult('a' -> "bc")
     }

  val `abcd` = CharacterParser.string("abcd")
  var choiceParser: Parser[String] = null

  """|## Choice parser
     |
     |This parser will try to parse input using all of the other parser.
     |
     |Below an example of a parser that gives an ambiguous result.
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
       choiceParser parse "" is Left(ExpectedInput)
       choiceParser parse "def" is Left(InvalidInput)
     }
     "- It returns the correct value if one of the parsers matched the input" - {
       choiceParser parse "abce" must beResult("abc" -> "e")
     }
     "- It returns multiple values if more than one matches the input\n " - {
       choiceParser parse "abcde" must beResult("abc" -> "de", "abcd" -> "e")
     }
     "nested choices" - {}

     var sequenceParser: Parser[View[String]] = null

  """|## Sequence parser
     |
     |This parser consumes the input using the given sequence of other parsers. It will then
     |convert to the specified result.
     |
     |Below a parser that is set up for ambiguity.
     | """.stripMargin - sideEffectExample {
       def takeExactly3(i: InvariantView[Char]): Split[Char] = {
         val x @ Split(a, b) = i.splitAt(Index(3))
         if (a.force.size == Size(3)) x
         else Split(emptyValue[View[Char]], i)
       }

       val take3 = CharacterParser(takeExactly3, _.mkString(""))

       val choice = ChoiceParser(Direct(`abc`, take3), identity[String])

       sequenceParser =
         SequenceParser(
           parsers = Direct(choice, `abcd`),
           toValue = identity[View[String]]
         )
     }

     "Note that this parser throws an exception if no parsers were given during construction" - {
       SequenceParser(emptyValue[View[Parser[String]]], identity[View[String]]) must throwA[Throwable]
     }
     "- It returns a failure when any of the given parsers returns a failure" - {
       sequenceParser parse ""     is Left(ExpectedInput)
       sequenceParser parse "d"    is Left(InvalidInput)
       sequenceParser parse "abc"  is Left(ExpectedInput)
       sequenceParser parse "abcd" is Left(InvalidInput)
       "[Note to self] Add position information" - {}
     }
     "- It returns a result when all of the given parsers return a result" - {
       val result1 = sequenceParser parse "abcabcd"
       result1 must beResult(newView("abc", "abcd") -> "", newView("abc", "abcd") -> "")
       val result2 = sequenceParser parse "123abcde"
       result2 must beResult(newView("123", "abcd") -> "e")
     }
     "- It correctly passes the remaining characters if they for some combination" - {
       val parser = SequenceParser(
         parsers = Direct(choiceParser, choiceParser),
         toValue = identity[View[String]]
       )
       parser parse "abcabcd"  must beResult(newView("abc", "abc")  -> "d", newView("abc", "abcd") -> "")
       parser parse "abcdabcd" must beResult(newView("abcd", "abc") -> "d", newView("abcd", "abcd") -> "")
       parser parse "abcdabc"  must beResult(newView("abcd", "abc") -> "")
       parser parse "abcabc"   must beResult(newView("abc", "abc")  -> "")
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
           underlying = CharacterParser.char('x'),
           toValue    = _.force[String]
         )
     }
     "- It will return a failure if the input is empty" - {
       notParser parse "" is Left(ExpectedInput)
     }
     "- It will not consume anything if the underlying parser consumed something" - {
       notParser parse "x" must beResult("" -> "x")
     }
     "- It consumes if the underlying parser fails to do so" - {
       notParser parse "yyy" must beResult("yyy" -> "")
     }
     "- It stops consuming as soon as the underlying parser starts to consume" - {
       notParser parse "yyyxxx" must beResult("yyy" -> "xxx")
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
       zeroOrOneParser parse "" must beResult(expected -> "")
     }
     "- It will return the correct remaining characters on a mismatch" - {
       val expected: Option[String] = None
       zeroOrOneParser parse "1" must beResult(expected -> "1")
     }
     "- It returns the results of the parser on a match" - {
       zeroOrOneParser parse "abcd" must beResult(Option("abc") -> "d", Option("abcd") -> "")
     }
   }
}
