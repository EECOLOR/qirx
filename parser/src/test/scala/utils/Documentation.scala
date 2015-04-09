package utils

import org.qirx.littlespec.Specification
import psp.api._
import psp.std.scSeq

trait Documentation extends Specification with CustomMatchers with CustomExamples {
  /* required for the location macro of little-spec */
  protected[this] val Seq = psp.std.scSeq
}
