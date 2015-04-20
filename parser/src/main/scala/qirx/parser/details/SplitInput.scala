package qirx.parser
package details

import psp.std.String

case class SplitInput(consumed: Input, remaining: Input) {

  def toOutcome(ifRejected: => String): Outcome =
    if (consumed.isEmpty) Rejected(ifRejected)
    else Consumed(consumed.underlying, remaining)
}
