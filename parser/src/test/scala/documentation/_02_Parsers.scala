package documentation

import psp.api._
import psp.std.{Failure => _, _}
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
   |- ???
   | """.stripMargin - {

  """|A parser is a type with the following signature
     | """.stripMargin - sideEffectExample {
       import qirx.parser.{Either => |}
       type ??? = Any
       val parser = new Parser[???] {
         def parse(input: InvariantView[Char]): Failure | View[Result[???]] = ???
       }
     }

     case class CustomResult(content: String)
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
     |Below ...
     | """.stripMargin - sideEffectExample {
       choiceParser =
         ChoiceParser(
           parsers = Direct(`abc`, `abcd`),
           toValue = identity[String]
         )
     }
     "- It throws an exception if no parsers were given during construction" - {
       ChoiceParser(emptyValue[View[Parser[String]]], identity[String]) must throwA[Throwable]
     }
     "- It returns a failure if there none of the parsers match the input" - {
       choiceParser parse "" is Left(ExpectedInput)
       choiceParser parse "def" is Left(InvalidInput)
     }
     "- It returns the correct value if one of the parsers matched the input" - {
       choiceParser parse "abce" must beResult("abc" -> "e")
     }
     "- It returns multiple values if more than one matches the input" - {
       choiceParser parse "abcde" must beResult("abc" -> "de", "abcd" -> "e")
     }
   }
}
