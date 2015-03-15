package qirx.parser

import psp.api._
import psp.std._

sealed trait Either[+A, +B] {
  def map[C](f: B => C): A | C = this match {
    case Left(value) => Left(value)
    case Right(value) => Right(f(value))
  }
  def flatMap[C, AA >: A](f: B => AA | C): AA | C = this match {
    case Left(value) => Left(value)
    case Right(value) => f(value)
  }
}

case class Left[+A](value: A)  extends Either[A, Nothing]
case class Right[+A](value: A) extends Either[Nothing, A]
