package utils

import psp.api.View
import psp.std._

trait Enforcer[A] {
  def apply(a:A):A
}
trait LowerPriorityEnforcers {
  implicit def any[A]:Enforcer[A] = new Enforcer[A] {
    def apply(a:A):A = a
  }
}
object Enforcer extends LowerPriorityEnforcers {
  implicit def view[A](implicit force: Enforcer[A]):Enforcer[View[A]] = new Enforcer[View[A]] {
    def apply(a:View[A]):View[A] = (a map force.apply).force
  }
}
