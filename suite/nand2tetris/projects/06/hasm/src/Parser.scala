package hasm

object Parser:
  type Result = Instruction | Error | Null

  private val labelRegex = """^\((.+)\)$""".r
  private val aInstructionRegex = """^@(.+)$""".r
  private val cInstructionRegex = """^([^;=]+=)?([^;=]+)(;.+)?$""".r

  def parseAll(sourceFile: os.Path): Vector[Result] =
    os.read
      .lines(sourceFile)
      .map(parseLine)
      .toVector

  def parseLine(line: String): Result =
    line.trim.nn match
      case line if line.isEmpty() || line.startsWith("//") => null
      case labelRegex(symbol)                              => Instruction.L(symbol)
      case aInstructionRegex(symbol)                       => Instruction.A(symbol)
      case cInstructionRegex(dest, comp, jump) =>
        Option(comp).fold(Error.InvalidInstruction(line)): _ =>
          Instruction.C(
            Option(dest).map(_.init), // ignore the last `=`
            comp,
            Option(jump).map(_.tail) // ignore the first `;`
          )
      case _: String => Error.InvalidInstruction(line)
