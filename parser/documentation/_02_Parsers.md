**This documentation is generated from `documentation._02_Parsers`**

---
#Parsers

We choose to create new parsers in order to have full control over over the readability
of the code.

These parsers can handle ambiguity which allows us to use them for grammar checking.

The parsers correspond do the elements in grammer:

- The character parser can be used to parse `Terminal` elements
- ???
 
A parser is a type with the following signature
 
```scala
import qirx.parser.{Either => |}
type ??? = Any
val parser = new Parser[???] {
  def parse(input: InvariantView[Char]): Failure | View[Result[???]] = ???
}
```
## Character parser

This is a parser that consumes characters and converts it to the specified result.

Below a simple definition that consumes any 'a' or 'b' character and converts its
retult to a custom case class.
 
```scala
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
```
- It returns a failure on empty input
- It returns a failure if the parser did not consume anything
- When it consumes input it will return the value together with the unconsumed input
 
