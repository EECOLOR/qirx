package qirx.parser

trait Failure {
  def input: Input
}

case class ExpectedInput(input: Input) extends Failure
case class InvalidInput (input: Input) extends Failure
