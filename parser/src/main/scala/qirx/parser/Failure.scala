package qirx.parser

import psp.std.String

sealed trait Failure {
  def input   : Input
  def message : String
}

case class ExpectedInput(input: Input) extends Failure {
  val message = "Expected input, but no input was provided"
}
case class InvalidInput(input: Input, message: String) extends Failure
