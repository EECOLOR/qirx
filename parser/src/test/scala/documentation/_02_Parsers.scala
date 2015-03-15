package documentation

import psp.api._
import psp.std._
import org.qirx.littlespec.Specification
import org.qirx.littlespec.fragments.Fragment
import org.qirx.littlespec.io.Source
import org.qirx.littlespec.macros.Location
import qirx.parser.Parser
import qirx.parser.Failure
import qirx.parser.Result
import qirx.parser.parsers.CharacterParser
import qirx.parser.ExpectedInput
import qirx.parser.InvalidInput
import qirx.parser.Left
import qirx.parser.Right
import qirx.parser.Result

object _02_Parsers extends Specification {

  /* required for the location macro of little-spec */
  protected[this] val Seq = psp.std.scSeq

  def sideEffectExample(code: => Unit)(implicit location: Location): Fragment =
    createFragment(Source.codeAtLocation(location), { code ; success} )

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

     var characterParser: CharacterParser[_] = null

  """|## Character parser
     |
     |This is a parser that consumes characters and converts it to the specified result.
     |
     |Below a simple definition that consumes any 'a' or 'b' character and converts its
     |retult to a custom case class.
     | """.stripMargin - sideEffectExample {
       def customConsume(characters: InvariantView[Char]): SplitView[Char] =
         characters.span(c => c == 'a' || c == 'b')

       case class CustomResult(content: String)

       def customToValue(consumed: View[Char]): CustomResult =
         CustomResult(consumed.mkString(""))

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
       characterParser parse "bac" match {
         case Left(failed)   => failure("Expected success, got: " + failed)
         case Right(results) =>
           results.size is Size(1)
           val result = results.head
           result.value.toString is "CustomResult(ba)"
           result.remaining.force is "c"
       }
     }
  }
}
