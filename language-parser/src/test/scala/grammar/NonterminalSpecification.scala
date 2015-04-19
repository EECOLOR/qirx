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
    parser parse "123.456" resultsIn ast.Decimal("123", Some("456"))
    parser parse "0x2F3AE26" resultsIn ast.Hex("2F3AE26")
  }

  "StringValue" - {
    val parser = grammar parserFor String

    parser parse "\"test\"" resultsIn ast.StringValue("test")
    parser parse "\"te\\\"st" resultsIn ast.StringValue("te\"st")
  }
}
