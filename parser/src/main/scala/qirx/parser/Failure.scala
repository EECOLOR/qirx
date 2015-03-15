package qirx.parser

trait Failure

case object ExpectedInput extends Failure
case object InvalidInput  extends Failure
