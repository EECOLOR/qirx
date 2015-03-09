package documentation

import org.qirx.littlespec.Specification
import org.qirx.littlespec.fragments.Fragment
import org.qirx.littlespec.io.Source
import org.qirx.littlespec.macros.Location
import psp.api._
import psp.std._

trait Documentation extends Specification {
  /* required for the location macro of little-spec */
  protected[this] val Seq = psp.std.scSeq

  def sideEffectExample(code: => Unit)(implicit location: Location): Fragment =
    createFragment(Source.codeAtLocation(location), { code ; success} )

  def withValue[T](value:T)(each: Each[T => FragmentBody]):FragmentBody = {
    each.foreach(_ apply value)
    success
  }
}
