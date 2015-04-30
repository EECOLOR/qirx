package qirx.language

import psp.api._
import psp.std._
import qirx.language.details.CodePoint
import qirx.parser.grammar.Custom

case class NotOrEscaped[T <: GroupMarker](value: T) extends Custom

class SingleChar private(val chars: ToBool[Char], val charsAsString: String) extends Custom {
  def this(char: Char) = this(_ === char, char.toString)
  def this(direct: Direct[Char]) = this(direct.to[ExSet], direct mkString ", ")
  def this(charsAsString: String, f: CodePoint => Boolean) = this((_: Char) |> CodePoint.apply |> f, charsAsString)
}
