package qirx.parser

case class Result[+A](value: A, remaining: Input) {
  def map[B](f: A => B): Result[B] = Result(f(value), remaining)
}
