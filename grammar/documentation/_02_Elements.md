**This documentation is generated from `documentation._02_Elements`**

---
# Elements

The elements can be roughly devided into 3 categories:

1. Parser instructions
2. Nonterminals
3. Terminals
 
## Parser instructions

These are the basic build blocks that can be used by a parser.
 
### Sequence

This type is associated with the ~ operation.
 
Calling the ~ method on any element will create a sequence instance
```scala
x ~ x is Sequence(x, x)
```
The operation is implemented in a way that prevents directly nested sequences.
 
### Choice

This type is associated with the | operation.
 
Calling the | method on any element will create a choice instance
```scala
x | x is Choice(x, x)

```
The operation is implemented in a way that prevents directly nested choices.
 
Choice and sequence can be combined. Sequence has higher priority.
```scala
x | x | x ~ x ~ x | x | x  is  x | x | (x ~ x ~ x) | x | x
x ~ x ~ x | x | x ~ x ~ x  is  (x ~ x ~ x) | x | (x ~ x ~ x)
```
### Attributed element

This is an element that allows you to decorate other elements. Note that by default
all of the attributes are set to `false`.
 
#### The + (one or more) operator
```scala
val a = x.+
a.element   is x
a.oneOrMore is true
```
#### The * (zero or more) operator
```scala
val a = x.*
a.element    is x
a.zeroOrMore is true
```
#### The ? (zero or one) operator
```scala
val a = x.?
a.element   is x
a.zeroOrOne is true
```
#### The ! (not) operator
```scala
val a = !x
a.element is x
a.not     is true
```
## Nonterminals

These are the elements that will contain other elements in the resulting grammar. You
are free to define any nonterminal element. Check the productions section of the
documentation to see how they are used.

```scala
case object MyNonterminal extends Nonterminal
val e: Element = MyNonterminal
```
## Terminals

These are the elements that will eventually be translated to sequences of characters. To
clearly distinguish them from nonterminals we have the convention to write them with
backticks and start their name with a lower case letter.

```scala
case object `myterminal` extends Terminal
val e: Element = `myterminal`
```