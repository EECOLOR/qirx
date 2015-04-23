package qirx.language

import psp.api._
import psp.std._

object NonterminalSpecification extends GrammarSpecification {

  "Decimal" - {
    val parser = grammar parserFor Decimal

    parser parse "123" resultsIn ast.Decimal("123", None)
    parser parse "123.456" resultsIn ast.Decimal("123", Some("456"))
  }

  "Hex" - {
    val parser = grammar parserFor Hex

    parser parse "0x2F3AE26" resultsIn ast.Hex("2F3AE26")
  }

  "Numeric" - {
    val parser = grammar parserFor Numeric

    parser parse "123" resultsIn ast.Decimal("123", None)
    parser parse "0x2F3AE26" resultsIn ast.Hex("2F3AE26")
  }

  "StringValue" - {
    val parser = grammar parserFor String

    parser parse "\"test\"" resultsIn ast.StringValue("test")
    parser parse "\"te\\\"st\"" resultsIn ast.StringValue("te\"st")
  }

  "CharValue" - {
    val parser = grammar parserFor Char

    parser parse "'test'"   resultsIn ast.CharValue("test")
    parser parse "'te\\'st'" resultsIn ast.CharValue("te'st")
  }

  "BooleanValue" - {
    val parser = grammar parserFor Boolean

    parser parse "true"  resultsIn ast.BooleanValue(`true`)
    parser parse "false" resultsIn ast.BooleanValue(`false`)
  }

  "NullValue" - {
    val parser = grammar parserFor Null

    parser parse "null" resultsIn ast.NullValue()
  }

  "Literal" - {
    val parser = grammar parserFor Literal

    parser parse "123"      resultsIn ast.Decimal("123", None)
    parser parse "\"test\"" resultsIn ast.StringValue("test")
    parser parse "'test'"   resultsIn ast.CharValue("test")
    parser parse "true"     resultsIn ast.BooleanValue(`true`)
    parser parse "null"     resultsIn ast.NullValue()
  }

  "Underscore" - {
    val parser = grammar parserFor Underscore

    parser parse "_" resultsIn ast.Underscore()
  }
}
