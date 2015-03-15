package qirx.parser

import psp.api._
import psp.api.Api._

sealed trait Either[+A, +B]

case class Left[+A](value: A)  extends Either[A, Nothing]
case class Right[+A](value: A) extends Either[Nothing, A]
