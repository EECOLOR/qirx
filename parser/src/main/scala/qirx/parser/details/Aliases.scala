package qirx.parser
package details

import psp.api.View

trait Aliases {
  type |[+A, +B] = ParseResult[A, B]

  type Results[+A] = View[Result[A]]
}
