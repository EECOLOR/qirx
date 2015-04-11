package qirx.parser

case class Result[+A](value: A, position: Position, remaining: Input) {
  def map[B](f: A => B): Result[B] = Result(f(value), position, remaining)
}
