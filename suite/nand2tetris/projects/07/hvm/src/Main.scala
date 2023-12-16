//> using scala 3.3.1
//> using toolkit latest
//> using option -Yexplicit-nulls

package hvm

import scala.util.chaining.*

// scala-cli . -- $(pwd)/../StackArithmetic/StackTest/StackTest  $(pwd)/../StackArithmetic/StackTest/StackTest
@main
def run(source: String, dest: String): Unit =
  HVM.translateToString(source, dest)

object HVM:
  type Result = Either[Vector[Error], Vector[String]]

  def translateToString(source: String, dest: String): Unit =
    val srcPath = os.Path(s"$source.vm")
    val destPath = os.Path(s"$dest.asm")

    Parser.parseAll(srcPath).pipe(translate(srcPath.last, _)) match
      case Left(errors) =>
        println(s"Could not translate the program: $errors")
      case Right(outputLines) =>
        os.write.over(destPath, outputLines.mkString("\n"))

  def translate(fileName: String, sourceLines: Vector[(Int, Parser.Result)]): Result =
    val fileNameWithoutExt = fileName.dropRight(3)
    val result = sourceLines.collect:
      case (_, e: Error)          => e
      case (lineNr, cmd: Command) => HASMWriter.writeCmd(cmd, fileNameWithoutExt, lineNr)

    val errors = result.collect:
      case e: Error => e

    if errors.nonEmpty then Left(errors) else Right(result.asInstanceOf[Vector[String]])

enum Command:
  case Arithmetic(name: String)
  case Push(a0: String, a1: Int)
  case Pop(a0: String, a1: Int)
  case Label(name: String)
  case Goto(label: String)
  case If(label: String)
  case Function(a0: String, a1: Int)
  case Return
  case Constant(value: Int)
  case Call(a0: String, a1: Int)

enum Error:
  case InvalidCommand(msg: String)

object Parser:
  type Result = Command | Error | Null

  // regex for three groups: command, arg1, arg2
  private val commandRegex = """^(\S+)(\s\S+)?(\s\S+)?$""".r

  private val arithmeticCommands = Set("add", "sub", "neg", "eq", "gt", "lt", "and", "or", "not")

  def parseAll(sourceFile: os.Path): Vector[(Int, Result)] =
    os.read
      .lines(sourceFile)
      .zipWithIndex
      .map: (line, i) =>
        (i, parseLine(line))
      .toVector

  def parseLine(line: String): Result =
    lazy val invalidCmd = Error.InvalidCommand(line)

    line.trim.nn match
      case line if line.isEmpty() || line.startsWith("//") => null
      case commandRegex(command, arg0, arg1) =>
        def withIntArg(arg: String): (Int => Result) => Result =
          arg.toIntOption.fold(invalidCmd)

        (command, Option(arg0).map(_.trim.nn), Option(arg1).map(_.trim.nn)) match
          case (cmd, _, _) if arithmeticCommands(cmd) => Command.Arithmetic(cmd)
          case ("label", Some(arg0), _)               => Command.Label(arg0)
          case ("goto", Some(arg0), _)                => Command.Goto(arg0)
          case ("if-goto", Some(arg0), _)             => Command.If(arg0)
          case ("return", _, _)                       => Command.Return
          case ("push", Some(arg0), Some(arg1))       => withIntArg(arg1)(Command.Push(arg0, _))
          case ("pop", Some(arg0), Some(arg1))        => withIntArg(arg1)(Command.Pop(arg0, _))
          case ("function", Some(arg0), Some(arg1))   => withIntArg(arg1)(Command.Function(arg0, _))
          case ("call", Some(arg0), Some(arg1))       => withIntArg(arg1)(Command.Call(arg0, _))
          case _                                      => invalidCmd
      case _ => invalidCmd

object HASMWriter:
  type Result = String | Error

  def writeCmd(cmd: Command, fileName: String, lineNr: Int): Result =
    cmd match
      case Command.Arithmetic(name)   => IntegerArithmetic.all(name, lineNr)
      case Command.Push(segment, pos) => StackArithmetic.push(segment, pos, fileName)
      case Command.Pop(segment, pos)  => StackArithmetic.pop(segment, pos, fileName)

      case _ => Error.InvalidCommand("Not implemented")

  // push segment i -> pushes the value of segment[i] onto the stack
  // pop segment i -> pops the value at the top of the stack and stores it in segment[i]
  // @SP holds the address of the RAM entry just after the topmost of the stack
  object StackArithmetic:
    def push(segment: String, pos: Int, fileName: String): Result =
      segment match
        case "constant" => pushConstant(pos)
        case "local"    => pushToSegmentAt("@LCL", pos)
        case "argument" => pushToSegmentAt("@ARG", pos)
        case "this"     => pushToSegmentAt("@THIS", pos)
        case "that"     => pushToSegmentAt("@THAT", pos)
        case "pointer" =>
          pos match
            case 0 => pushToSegment("@THIS")
            case 1 => pushToSegment("@THAT")
            case _ => Error.InvalidCommand(s"Invalid pointer position: $pos")
        case "temp"   => temp(pos, pushToSegment)
        case "static" => pushStatic(pos, fileName)
        case _        => Error.InvalidCommand(s"Invalid segment: $segment")

    def pop(segment: String, pos: Int, fileName: String): Result =
      segment match
        case "constant" => Error.InvalidCommand("Cannot pop to constant")
        case "local"    => popFromSegmentAt("@LCL", pos)
        case "argument" => popFromSegmentAt("@ARG", pos)
        case "this"     => popFromSegmentAt("@THIS", pos)
        case "that"     => popFromSegmentAt("@THAT", pos)
        case "pointer" =>
          pos match
            case 0 => popFromSegment("@THIS")
            case 1 => popFromSegment("@THAT")
            case _ => Error.InvalidCommand(s"Invalid pointer position: $pos")
        case "temp"   => temp(pos, popFromSegment)
        case "static" => popStatic(pos, fileName)
        case _        => Error.InvalidCommand(s"Invalid segment: $segment")

    private def temp(pos: Int, fn: String => String) =
      // RAM location 5 - 12
      if pos < 0 || pos > 7 then Error.InvalidCommand(s"Invalid temp position: $pos") else fn(s"@R${5 + pos}")

    // requires setting `D` for a value to push
    // RAM[SP] = D; SP++
    private def pushToStack = "@SP" \
      "AM=M+1" \ // increment stack pointer and point to next empty cell
      "A=A-1" \ // point to the previous top of stack
      "M=D" // set top of stack to value from `D`

    private def pushStatic(pos: Int, fileName: String) = pushToSegment(s"@$fileName.$pos")

    private def pushConstant(value: Int) =
      s"@$value" \
        "D=A" \ // load constant into D
        pushToStack

    private def pushToSegmentAt(segment: String, offset: Int) =
      pushToSegment(segmentOffset(segment, offset))

    private def pushToSegment(segment: String) =
      segment \
        "D=M" \ // load value from segment
        pushToStack

    // sets `D` to value from top of stack
    // SP--;  D = RAM[SP]
    private def popFromStack = "@SP" \
      "AM=M-1" \ // decrement stack pointer and point to next value on stack
      "D=M" // set value from stack to `D`

    private def popFromSegment(segment: String) = segment \
      "D=A" \ // save segment address in D
      "@R13" \
      "M=D" \ // save segment address in R13
      popFromStack \
      "@R13" \
      "A=M" \
      "M=D" // set value from `D` to segment

    private def popFromSegmentAt(segment: String, offset: Int) =
      popFromSegment(segmentOffset(segment, offset))

    // sets `A` to segment address + offset
    private def segmentOffset(name: String, offset: Int) =
      s"@$offset" \
        "D=A" \ // load constant into D
        name \
        "A=M+D" // point to segment (base address + offset)

    private def popStatic(pos: Int, fileName: String) = popFromSegment(s"@$fileName.$pos")

  object IntegerArithmetic:
    def all(cmd: String, n: Int): Result =
      cmd match
        case "add" => popTwo \ "M=D+M"
        case "sub" => popTwo \ "M=M-D"
        case "neg" => popOne \ "M=-M"
        case "and" => popTwo \ "M=D&M"
        case "or"  => popTwo \ "M=D|M"
        case "not" => popOne \ "M=!M"
        case "eq"  => cmp("JEQ", n)
        case "gt"  => cmp("JGT", n)
        case "lt"  => cmp("JLT", n)

    private def cmp(code: String, i: Int) = popTwo \
      "D=M-D" \ // subtract 2nd value from 1st
      //
      s"@ON_TRUE_$i" \ // if True, then jump to ON_TRUE_$i
      s"D;$code" \
      // --- False ---
      "@SP" \
      "A=M-1" \
      "M=0" \
      s"@CHECK_END_$i" \ // jump to CHECK_END_$i
      "0;JMP" \
      // -- True ---
      s"(ON_TRUE_$i)" \
      "@SP" \
      "A=M-1" \
      "M=-1" \
      //
      s"(CHECK_END_$i)"

    // set `D` to value from top of stack and `M` to value from 2nd top of stack
    private val popTwo = "@SP" \
      "AM=M-1" \ // decrement stack pointer and point to next value on stack
      "D=M" \ // save value from stack in D
      "A=A-1" // point to next value on stack

    // pop & push
    private val popOne = "@SP" \ "A=M-1"

  extension (s: String) inline def \(inline line: String) = s + "\n" + line
