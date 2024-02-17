package jackc

import scala.util.chaining.*

opaque type VMCode = String
object VMCode:
  def apply(code: String): VMCode = code

  extension (sc: StringContext) def hvm(args: Any*): VMCode = sc.s(args*).pipe(VMCode.apply)
  extension (code: Vector[VMCode]) def collapse: String = code.mkString("\n")
