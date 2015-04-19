**This documentation is generated from `documentation._02_Grammar_elements`**

---
# Grammar

The elements in a grammar can be roughly devided into 3 categories:

1. Parser instructions
2. Nonterminals
3. Terminals
4. Custom
 
## Parser instructions

These are the basic build blocks that can be used by a parser.
 
### Sequence

This type is associated with the ~ operation.
 
Calling the ~ method on any element will create a sequence instance
```scala
x ~ x is Sequence(x :: x :: HNil)
```
The operation is implemented in a way that prevents directly nested sequences.
 
### Choice

This type is associated with the | operation.
 
Calling the | method on any element will create a choice instance
```scala
x | x is Choice(x :: x :: HNil)

```
The operation is implemented in a way that prevents directly nested choices.
 
Choice and sequence can be combined. Sequence has higher priority.
```scala

         x | x | x ~ x ~ x | x | x  is  x | x | (x ~ x ~ x) | x | x
         x ~ x ~ x | x | x ~ x ~ x  is  (x ~ x ~ x) | x | (x ~ x ~ x)
      
```
### The + (one or more) operator
```scala
x.+ is OneOrMore(x)
```
### The * (zero or more) operator
```scala
x.* is ZeroOrMore(x)
```
### The ? (zero or one) operator
```scala
x.? is ZeroOrOne(x)
```
### The ! (not) operator
```scala
!x is Not(x)
```
## Nonterminals

These are the elements that will contain other elements in the resulting grammar. You
are free to define any nonterminal element. Check the productions section of the
documentation to see how they are used.
 
```scala
case object Statement extends Nonterminal[SomeType]
val e: Element = Statement
```
## Terminals

These are the elements that will eventually be translated to sequences of characters.

By convention we write terminals that will be translated to a fixed set of characters
with backticks and in lower case. Terminals that will be translated to an arbitrary
set of characters are written with standard convention.

We have devided the types of terminals into two categories
 
### Variable

This marks the terminal as being able to accept an arbitrary set of characters.
 
#### Free

This marks the terminal as capturing an abitrary set of characters
 
```scala
case object Id extends Free

Id must be[Variable with Capture[String]]
```
#### Scrap

This marks the terminal as being able to accept an arbitrary set of characters, but
they are not of any value.
 
```scala
case object Whitespace extends Scrap

Whitespace must be[Variable with Capture[Unit]]
```
### Fixed

This marks the terminal as being able to accept a fixed set of characters.
 
#### Keyword

Use this to mark a terminal as a keyword, not capturing anything
 
```scala
case object `keyword` extends Keyword

`keyword` must be[Fixed with Capture[Unit]]
```
#### Feature

Use this to mark a terminal as a feature, capturing the object itself
 
```scala
case object `feature` extends Feature

`feature` must be[Fixed with Capture[Capture.Self]]
```
## Custom

This trait is not used by the library. But since `Element` is sealed, you can use this
to create your own and hook into the available features.
 
```scala
case object CustomElement extends Custom
```
#### Literal

This marks the terminal as being a literal.
 
```scala
case object `\"` extends Literal
```
