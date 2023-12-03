//> using scala 3.3.1
//> using toolkit latest
//> using option -Yexplicit-nulls

package hasm

import scala.util.control.NoStackTrace

@main
def run =
  println(
    Parser.parseLine("M=A|1")
  )

object HASM:
  /** A 15-bit ROM (for instructions) or RAM.
    */
  opaque type Address = Int
  object Address:
    def initial: Address = 0
    def apply(address: Int): Address = address
  extension (address: Address)
    def next: Address = address + 1

  final case class State(
      symbolTable: Map[String, Address],
      labelAddress: Address,
      lines: Vector[Instruction]
  ):
    def incrementLine = copy(labelAddress = labelAddress.next)
  object State:
    def initial: State =
      val defaultSymbols = (0 to 15).map(i => s"R$i" -> Address(i)).toMap ++
        Map(
          "SP" -> Address(0),
          "LCL" -> Address(1),
          "ARG" -> Address(2),
          "THIS" -> Address(3),
          "THAT" -> Address(4),
          "SCREEN" -> Address(16384),
          "KBD" -> Address(24576)
        )

      State(defaultSymbols, Address.initial, Vector.empty)

  // TODO: First pass, page 172
  def assemble(sourceFile: String): Unit =
    os.read
      .lines(os.Path(sourceFile))
      .foldLeft(State.initial): (state, line) =>
        // println(line)
        // state
        // Parser.parseLine(line) match
        //   case inst @ Instruction.L(symbol) => println(s"Label: $symbol")
        //   case inst @ Instruction.A(symbol) => println(s"A: $symbol")
        //   case inst @ Instruction.C(dest, comp, jump) =>
        //     println(s"C: $dest, $comp, $jump")
        //   case Error.InvalidInstruction(line, comment) =>
        //     println(s"Invalid instruction: $line")
        //   case null => println("Empty line")
        //   case _ => println("Unknown instruction")
        ???

    ???
    // val lines = io.Source.fromFile(sourceFile).getLines().toVector
    // val instructions = lines.map(Parser.parseLine)
    // println(instructions)

enum Instruction:
  case L(symbol: String)
  case A(symbol: String)
  case C(dest: Option[String], comp: Option[String], jump: Option[String])

enum Error extends NoStackTrace:
  case InvalidInstruction(line: String, comment: Option[String] = None)

object Parser:
  type Result = Instruction | Error | Null

  private val labelRegex = """^\((.+)\)$""".r
  private val aInstructionRegex = """^@(.+)$""".r
  private val cInstructionRegex = """^([^;=]+=)?([^;=]+)(;.+)?$""".r

  def parseLine(line: String): Result =
    line.trim.nn match
      case line if line.isEmpty() || line.startsWith("//") => null
      case labelRegex(symbol)        => Instruction.L(symbol)
      case aInstructionRegex(symbol) => Instruction.A(symbol)
      case cInstructionRegex(dest, comp, jump) =>
        Instruction.C(
          Option(dest).map(_.init), // ignore the last `=`
          Option(comp),
          Option(jump).map(_.tail) // ignore the first `;`
        )
      case _ => Error.InvalidInstruction(line)
