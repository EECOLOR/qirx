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

def customToValue(consumed: View[Char]): CustomResult =
  CustomResult(consumed.force[String])

characterParser =
  CharacterParser(
    consume = customConsume,
    toValue = customToValue
  )
```
- It returns a failure on empty input
- It returns a failure if the parser did not consume anything
- When it consumes input it will return the value together with the unconsumed input
 
### String parser

We have supplied a string parser that can be used to match an exact string.
 
```scala
`abc` = CharacterParser.string("abc")
```
- It fails on input that does not start with the specified string
- It returns the string with no remaining input if it consumed all characters
- It returns the correct remaining input if it did not consume all characters
## Choice parser

This parser will try to parse input using all of the other parser.

Below ...
 
```scala
choiceParser =
  ChoiceParser(
    parsers = Direct(`abc`, `abcd`),
    toValue = identity[String]
  )
```
- It throws an exception if no parsers were given during construction
- It returns a failure if there none of the parsers match the input
- It returns the correct value if one of the parsers matched the input
- It returns multiple values if more than one matches the input
