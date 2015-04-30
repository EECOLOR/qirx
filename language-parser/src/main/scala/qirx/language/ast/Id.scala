package qirx.language.ast

import psp.std.String

trait Id {
  def value: String
}
case class RegularId(value: String) extends Id
case class EscapedId(value: String) extends Id

