package qirx.parser.raw

// TODO Move toString methods to Show[T] or Print[T] instances

sealed trait RawAst

case object NoCapture extends RawAst

case class StringValue(value: String) extends RawAst {
  override def toString = AsString("StringValue", value)
}
case class Id(value: String) extends RawAst {
  override def toString = AsString("Id", value)
}
case class Sequence(values: RawAst*) extends RawAst {
  override def toString = values.mkString("Sequence(\n  ", ",\n  ", "\n)")
}
object Sequence {
  def fromTuple(values: (RawAst, RawAst)): Sequence = {
    val (left, right) = values
    Sequence(left, right)
  }
}

object AsString {
  def apply(name:String, value:String): String = s"""$name("$value")"""
}
