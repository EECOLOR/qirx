package utils

import psp.api._
import psp.std._

trait Enforcer[A] {
  type Out
  def apply(a:A):Out
}
trait LowerPriorityEnforcers {
  implicit def any[A] = new Enforcer[A] {
    type Out = A
    def apply(a:A):A = a
  }
}
object Enforcer extends LowerPriorityEnforcers {
  implicit def view[A] = new Enforcer[View[A]] {
    type Out = Direct[A]
    def apply(a:View[A]):Direct[A] = a.force
  }
}
