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
       result1 must beResult(View("abc", "abcd") -> "", View("abc", "abcd") -> "")
       val result2 = sequenceParser parse "123abcde"
       result2 must beResult(View("123", "abcd") -> "e")
     }
     "- It correctly passes the remaining characters if they for some combination" - {
       val parser = SequenceParser(
         parsers = Direct(choiceParser, choiceParser),
         toValue = identity[View[String]]
       )
       parser parse "abcabcd" must beResult(View("abc", "abc") -> "d", View("abc", "abcd") -> "")
       parser parse "abcdabcd" must beResult(View("abcd", "abc") -> "d", View("abcd", "abcd") -> "")
       parser parse "abcdabc" must beResult(View("abcd", "abc") -> "")
       parser parse "abcabc" must beResult(View("abc", "abc") -> "")
     }
   }

}
