package qirx.language

import qirx.parser.grammar.Free
import qirx.parser.grammar.Keyword
import qirx.parser.grammar.Feature

trait Separator extends Keyword
trait GroupMarker extends Keyword

case object Digits extends Free
case object HexDigits extends Free

// The current implementation of back ticks is not really what it needs to be
case object `.` extends Separator
case object `"` extends GroupMarker // "
case object `0x` extends Keyword
