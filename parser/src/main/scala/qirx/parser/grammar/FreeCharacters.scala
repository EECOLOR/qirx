package qirx.parser.grammar

import psp.api.ExMap
import psp.api.ExSet
import psp.std.Char

trait FreeCharacters {
  def freeCharacters: ExMap[Free, ExSet[Char]]
}
