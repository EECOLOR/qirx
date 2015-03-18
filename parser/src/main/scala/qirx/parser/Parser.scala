package qirx.parser

import psp.api._
import psp.api.Api._

trait Parser[A] {
  def parse(input: Input): Failure | View[Result[A]]
}
