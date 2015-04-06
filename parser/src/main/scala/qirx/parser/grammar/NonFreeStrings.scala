package qirx.parser.grammar

import psp.api.ExMap
import psp.std.String

trait NonFreeStrings {
  def nonFreeStrings: ExMap[NonFree, String]
}
