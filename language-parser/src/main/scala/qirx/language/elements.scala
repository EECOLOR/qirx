package qirx.language

import qirx.parser.grammar.Custom

case class NotOrEscaped[T <: GroupMarker](value: T) extends Custom
