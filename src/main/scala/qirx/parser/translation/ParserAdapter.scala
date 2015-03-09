package qirx.parser.translation

import scala.language.higherKinds

trait ParserAdapter[T[_]] {

  def predicate[X](f: String => X)(predicate: Char => Boolean):T[X]
  def char[X](f: String => X)(char:Char):T[X]
  def negative[X](f: String => X)(element:T[_]):T[X]
  def sequence[X, Y](f: ((X, X)) => Y)(head: T[X], tail: T[X]):T[Y]

  def parse[X](adaptedGrammar: T[X], value:String):X
}
