package qirx.parser.grammar.details

final class Translate[-A, +B](f: A => B) {
  def apply(a:A):B = f(a)
}
