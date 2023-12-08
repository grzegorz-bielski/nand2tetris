package hasm

import scala.util.control.NoStackTrace

/** A 15-bit ROM (for instructions) or RAM.
  */
opaque type Address = Int
object Address:
  def initial: Address = 0
  def apply(address: Int): Address = address
extension (address: Address)
  def next: Address = address + 1
  def asBinaryString: String = address.toBinaryString

enum Instruction:
  case L(symbol: String)
  case A(symbol: String)
  case C(dest: Option[String], comp: String, jump: Option[String])

enum Error extends NoStackTrace:
  case InvalidInstruction(line: String, comment: Option[String] = None)
