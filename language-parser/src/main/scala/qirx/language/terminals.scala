package qirx.language

import qirx.parser.grammar.Free
import qirx.parser.grammar.Separator
import qirx.parser.grammar.Keyword
import qirx.parser.grammar.Feature
import qirx.parser.grammar.GroupMarker
import qirx.parser.grammar.Literal

case object Digits extends Free
case object HexDigits extends Free

// The current implementation of back ticks is not really what it needs to be
case object `.` extends Separator
case object `"` extends GroupMarker // "
case object ` \" ` extends Literal // "
case object `0x` extends Keyword
