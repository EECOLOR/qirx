package documentation

import psp.std._
import qirx.grammar.AttributedElement
import qirx.grammar.Choice
import qirx.grammar.Element
import qirx.grammar.Nonterminal
import qirx.grammar.Sequence
import qirx.grammar.Terminal

object _02_Elements extends Documentation {

"""|# Elements
   |
   |The elements can be roughly devided into 3 categories:
   |
   |1. Parser instructions
   |2. Nonterminals
   |3. Terminals
   | """.stripMargin - {

  """|## Parser instructions
     |
     |These are the basic build blocks that can be used by a parser.
     | """.stripMargin - {

     val unlikelySituation = AttributedElement(AttributedElement(null:Element)(false, false, false, false))(true, true, true, true)
     val x: Element = unlikelySituation

    """|### Sequence
       |
       |This type is associated with the ~ operation.
       | """.stripMargin - {
         "Calling the ~ method on any element will create a sequence instance" - example {
           x ~ x is Sequence(x, x)
         }
         "The operation is implemented in a way that prevents directly nested sequences.\n " - {
           (x ~ x) ~ x  is Sequence(x, x, x)
            x ~ (x ~ x) is Sequence(x, x, x)

           (x ~ x) ~ (x ~ x) is Sequence(x, x, x, x)
         }
       }

    """|### Choice
       |
       |This type is associated with the | operation.
       | """.stripMargin - {
         "Calling the | method on any element will create a choice instance" - example {
           x | x is Choice(x, x)

         }
         "The operation is implemented in a way that prevents directly nested choices.\n " - {
           (x | x) | x  is Choice(x, x, x)
            x | (x | x) is Choice(x, x, x)

           (x | x) | (x | x) is Choice(x, x, x, x)
         }
       }

       "Choice and sequence can be combined. Sequence has higher priority." - example {
         x | x | x ~ x ~ x | x | x  is  x | x | (x ~ x ~ x) | x | x
         x ~ x ~ x | x | x ~ x ~ x  is  (x ~ x ~ x) | x | (x ~ x ~ x)
       }

    """|### Attributed element
       |
       |This is an element that allows you to decorate other elements. Note that by default
       |all of the attributes are set to `false`.
       | """.stripMargin - {

         val a = AttributedElement(x)()
         a.zeroOrOne  is false
         a.oneOrMore  is false
         a.zeroOrMore is false
         a.not        is false

         "#### The + (one or more) operator" -example {
           val a = x.+
           a.element   is x
           a.oneOrMore is true
         }
         "#### The * (zero or more) operator" - example {
           val a = x.*
           a.element    is x
           a.zeroOrMore is true
         }
         "#### The ? (zero or one) operator" - example {
           val a = x.?
           a.element   is x
           a.zeroOrOne is true
         }
         "#### The ! (not) operator" - example {
           val a = !x
           a.element is x
           a.not     is true
         }
       }
     }

  """|## Nonterminals
     |
     |These are the elements that will contain other elements in the resulting grammar. You
     |are free to define any nonterminal element. Check the productions section of the
     |documentation to see how they are used.
     | """.stripMargin - sideEffectExample {
       case object Statement extends Nonterminal
       val e: Element = Statement
     }

  """|## Terminals
     |
     |These are the elements that will eventually be translated to sequences of characters.
     |
     |By convention we write terminals that will be translated to a fixed set of characters
     |with backticks and in lower case. Terminals that will be translated to an arbitrary
     |set of characters are written with standard convention.
     | """.stripMargin - sideEffectExample {
       case object `keyword` extends Terminal
       case object Id extends Terminal
       val e: (Element, Element) = (`keyword`, Id)
     }
   }
}
