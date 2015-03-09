package documentation

import psp.api._
import psp.std._
import psp.std.HashEq.universalEq
import psp.std.StdEq._
import qirx.grammar.Terminal
import qirx.grammar.Nonterminal
import qirx.grammar.Productions
import qirx.grammar.Element
import qirx.grammar.Sequence

object _03_Productions extends Documentation {

"""|# Productions
   |
   |A grammer consists of productions. In this particular project we focus mainly on the
   |productions of nonterminals. Our goal is to provide a facility to define production
   |with the minimal amount of noise.
   |""".stripMargin - {

  """|To define a set of productions you can extend the productions trait. This trait
     |allows you to define productions a clean way.
     |
     |Note that behind the screens a variable is used to the productions. This is perfectly
     |safe because we only use it in the constructor and do not expose it to the outside
     |world.
     |
     |In the following example we define a grammar that accepts arbitrary methods with
     |one or more strings as argument.
     | """.stripMargin - example {
       // Define the terminals
       case object `(` extends Terminal
       case object `)` extends Terminal
       case object `,` extends Terminal
       case object Id  extends Terminal
       case object `"` extends Terminal // "

       // Define the nonterminals
       case object Statement extends Nonterminal
       case object String    extends Nonterminal

       // Create the productions
       object Productions extends Productions {
         Statement := Id ~ `(` ~ String ~ (`,` ~ String).* ~ `)`
         String    := `"` ~ !`"` ~ `"`
       }

       withValue(Productions.productions)(
         Direct(
           _ must beAnInstanceOf[ExMap[Nonterminal, Element]],
           _ contains Statement is true,
           _ contains String is true,
           _(Statement) must beAnInstanceOf[Sequence],
           _(String)    must beAnInstanceOf[Sequence]
         )
       )
     }
   }

}
