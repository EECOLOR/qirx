package qirx.language

import qirx.parser.grammar.Free
import qirx.parser.grammar.Keyword
import qirx.parser.grammar.Feature

trait Separator extends Keyword
trait GroupMarker extends Keyword

case object Digits extends Free
case object HexDigits extends Free

sealed trait BooleanFeature extends Feature
case object `true`  extends BooleanFeature
case object `false` extends BooleanFeature

sealed trait `null` extends Feature
case object `null` extends `null`

sealed trait `__` extends Feature
case object `__`  extends `__`

// The current implementation of back ticks is not really what it needs to be
case object `.`  extends Separator
case object `"`  extends GroupMarker // "
case object `0x` extends Keyword
case object `\\` extends Keyword
case object `'`  extends GroupMarker // '
