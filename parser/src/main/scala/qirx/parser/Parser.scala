package qirx.parser

import psp.api.View

trait Parser[+A] {
  def parse(input: Input): Failure | View[Result[A]]
}
