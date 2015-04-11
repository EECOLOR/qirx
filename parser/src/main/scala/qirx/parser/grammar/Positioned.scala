package qirx.parser.grammar

import psp.std.Unit
import scala.annotation.implicitNotFound
import qirx.parser.Position

trait Positioned {

  /* The construction below is a bit unusual, so here's the comment that goes with it. I wanted
   * to make it as easy as possible for the end user to have position information in their AST.
   *
   * Potentially every element of the AST has a position when it's parsed, the problem is that
   * the AST is not under our control. So we needed to introduce a trait that contains the
   * position information. We did not want to pollute the user's AST by requiring them to declare
   * a position member. For this reason we added this mutable construction.
   *
   * If the user wants position information in their AST, they simply extend the `Positioned`
   * trait. We have added a simple lock to the setter of the position to help prevent accidental
   * use. It's not a real lock as you could always pass `null`, but it allows us to communicate
   * through the `implicitNotFound` annotation.
   */
  private var _position: Position = Position.None
  def position: Position = _position
  def position_=(position: Position)(implicit lock: Positioned.Key): Unit = _position = position
}

object Positioned {
  @implicitNotFound("Key is required to set the position. Note that you probably should not be trying to do this unless you really know what you are doing.")
  sealed trait Key
}
