**This documentation is generated from `documentation._02_Parsers`**

---
#Parsers

We choose to create new parsers in order to have full control over over the readability
of the code.

These parsers can handle ambiguity which allows us to use them for grammar checking.

The parsers correspond do the elements in grammer:

- The character parser can be used to parse `Terminal` elements
- The choice parser can be used to parse `Choice` elements
- The sequence parser can be used to parse `Sequence` (or `Nonterminal`) elements
- The not parser can be used to parse `Attributed.not` elements
- The zero or one parser can be used to parse `Attributed.zeroOrOne` elements
- The zero or more parser can be used to parse `Attributed.zeroOrMore` elements
- The one or more parser can be used to parse `Attributed.zeroOrOne` elements
 
[Note to self] Create property based tests for all parsers
> Pending: TODO

A parser is a type with the following signature
 
```scala
import qirx.parser.{Either => |}
type ??? = Any
val parser = new Parser[???] {
  def parse(input: Input): Failure | View[Result[???]] = ???
}
```
## Character parser

This is a parser that consumes characters and converts it to the specified result.

Below a simple definition that consumes any 'a' or 'b' character and converts its
retult to a custom case class.
 
```scala
def customConsume(characters: Input): SplitInput =
  characters.span(c => c == 'a' || c == 'b')

def customToValue(consumed: InvariantView[Char] with HasPreciseSize): CustomResult =
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
### Single character parser

We have supplied a character parser that can be used to match an exact string.
 
```scala
`a` = CharacterParser.char('a')
```
- It fails on input that does not start with the specified character
- It returns the character with no remaining input if it consumed all characters
- It returns the correct remaining input if it did not consume all characters
## Choice parser

This parser will try to parse input using all of the other parser.

Below an example of a parser that gives an ambiguous result.
 
```scala
choiceParser =
  ChoiceParser(
    parsers = Direct(`abc`, `abcd`),
    toValue = identity[String]
  )
```
Note that this parser throws an exception if no parsers were given during construction
- It returns a failure if there none of the parsers match the input
- It returns the correct value if one of the parsers matched the input
- It returns multiple values if more than one matches the input
 
- It correctly handles nested choice parsers
## Sequence parser

This parser consumes the input using the given sequence of other parsers. It will then
convert to the specified result.

Below a parser that is set up for ambiguity.
 
```scala
def takeExactly3(input: Input): SplitInput = {
  val result @ SplitInput(consumed, _) = input.splitAt(Precise(3))
  if (consumed.size == Size(3)) result
  else SplitInput(Input.empty, input)
}

val take3 = CharacterParser(takeExactly3, _.mkString(""))

val choice = ChoiceParser(Direct(`abc`, take3), identity[String])

sequenceParser =
  SequenceParser(
    parsers = Direct(choice, `abcd`),
    toValue = identity[View[String]]
  )
```
Note that this parser throws an exception if no parsers were given during construction
- It returns a failure when any of the given parsers returns a failure
- It returns a result when all of the given parsers return a result
- It correctly passes the remaining characters if they for some combination
## Not parser

This consumes input that the underlying parser did not accept. Note that it completely
ignores the successful result of the underlying parser.

Below a parser that consumes anything but the `x` character.
 
```scala
notParser =
  NotParser(
    underlying = CharacterParser.char('x'),
    toValue    = _.force[String]
  )
```
- It will return a failure if the input is empty
- It will not consume anything if the underlying parser consumed something
- It consumes if the underlying parser fails to do so
- It stops consuming as soon as the underlying parser starts to consume
## Zero or one parser

This parser tries the underlying parser on the input

Below a parser that tries the underlying parser, but will not fail if it fails.
 
```scala
zeroOrOneParser =
  ZeroOrOneParser(
    underlying = choiceParser,
    toValue    = identity[Option[String]]
  )
```
- It will not return an error if no input is available
- It will return the correct remaining characters on a mismatch
- It returns the results of the parser on a match
## Zero or more parser

This repeats the underlying parser zero or more times.

Below a parser that never fails and executes the underlying parser as often as it can.
 
```scala
zeroOrMoreParser =
  ZeroOrMoreParser(
    underlying = choiceParser,
    toValue    = identity[View[String]]
  )
```
- It will not return an error if no input is available
- It returns all remaining input if the underlying parser failed
- It executes the parser multiple times and be as greedy as it can be
[Note to self] think this through (see the above comment)
> Pending: TODO

## One or more parser

Repeats the underlying parser at least one time

This parser that consumes the underlying input at least once
 
```scala
oneOrMoreParser =
  OneOrMoreParser(
    underlying = choiceParser,
    toValue    = identity[View[String]]
  )
```
- It should return a failure when presented with no input
- It should report a failure when the underlying parser fails
- It succeeds if the underlying parser consumed at least once
- It succeeds if the underlying parser can consume multiple times
