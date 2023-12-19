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

  // regex for three groups: command, arg1?, arg2?
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
    lazy val withIntArg: String => (Int => Result) => Result = _.toIntOption.fold(invalidCmd)

    line.trim.nn match
      case line if line.isEmpty() || line.startsWith("//") => null
      case commandRegex(command, arg0, arg1) =>
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
      case _: String => invalidCmd

object HASMWriter:
  type Result = String | Error

  def writeCmd(cmd: Command, fileName: String, line: Int): Result =
    cmd match
      case Command.Arithmetic(name)   => IntegerArithmetic.from(name, fileName, line)
      case Command.Push(segment, pos) => StackArithmetic.run(StackArithmetic.Mode.Push, segment, pos, fileName)
      case Command.Pop(segment, pos)  => StackArithmetic.run(StackArithmetic.Mode.Pop, segment, pos, fileName)

      case _ => Error.InvalidCommand("Not implemented")

  // push segment i -> pushes the value of segment[i] onto the stack
  // pop segment i -> pops the value at the top of the stack and stores it in segment[i]
  // @SP holds the address of the RAM entry just after the topmost of the stack
  private object StackArithmetic:
    enum Mode:
      case Push, Pop

    def run(mode: Mode, segment: String, pos: Int, fileName: String): Result =
      segment match
        case "constant" => constant(mode, pos)
        case "local"    => segmentAt(mode, "@LCL", pos)
        case "argument" => segmentAt(mode, "@ARG", pos)
        case "this"     => segmentAt(mode, "@THIS", pos)
        case "that"     => segmentAt(mode, "@THAT", pos)
        case "pointer" =>
          pos match
            case 0 => segmentAt(mode, "@THIS")
            case 1 => segmentAt(mode, "@THAT")
            case _ => Error.InvalidCommand(s"Invalid pointer position: $pos")
        case "temp"   => temp(mode, pos)
        case "static" => static(mode, pos, fileName)
        case _        => Error.InvalidCommand(s"Invalid segment: $segment")

    private def temp(mode: Mode, pos: Int) =
      if pos < 0 || pos > 7 then Error.InvalidCommand(s"Invalid temp position: $pos. Available RAM locations: 5 - 12")
      else segmentAt(mode, s"@R${5 + pos}")

    // requires setting `D` for a value to push
    // RAM[SP] = D; SP++
    private def pushToStack = "@SP" \
      "AM=M+1" \ // increment stack pointer and point to next empty cell
      "A=A-1" \ // point to the previous top of stack
      "M=D" // set top of stack to value from `D`

    private def static(mode: Mode, pos: Int, fileName: String) = segmentAt(mode, s"@$fileName.$pos")

    private def constant(mode: Mode, value: Int): Result =
      mode match
        case Mode.Push => s"@$value" \ "D=A" \ pushToStack
        case Mode.Pop  => Error.InvalidCommand("Cannot pop to constant")

    private def segmentAt(mode: Mode, name: String, offset: Int): Result =
      segmentAt(mode, segmentOffset(name, offset))

    private def segmentAt(mode: Mode, name: String): Result =
      (
        mode match
          case Mode.Push => pushToSegment
          case Mode.Pop  => popFromSegment
      ).apply(name)

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

  private object IntegerArithmetic:
    def from(cmd: String, fileName: String, line: Int): Result =
      cmd match
        case "add" => popTwo \ "M=D+M"
        case "sub" => popTwo \ "M=M-D"
        case "neg" => popOne \ "M=-M"
        case "and" => popTwo \ "M=D&M"
        case "or"  => popTwo \ "M=D|M"
        case "not" => popOne \ "M=!M"
        case "eq"  => cmp("JEQ", fileName, line)
        case "gt"  => cmp("JGT", fileName, line)
        case "lt"  => cmp("JLT", fileName, line)

    // TODO: use filename ?
    private def cmp(code: String, fileName: String, line: Int) =
      val suffix = s"${fileName}_${line}"

      popTwo \
        "D=M-D" \ // subtract 2nd value from 1st
        //
        s"@ON_TRUE_$suffix" \ // if True, then jump to ON_TRUE_$i
        s"D;$code" \
        // --- False ---
        "@SP" \
        "A=M-1" \
        "M=0" \
        s"@CHECK_END_$suffix" \ // jump to CHECK_END_$i
        "0;JMP" \
        // -- True ---
        s"(ON_TRUE_$suffix)" \
        "@SP" \
        "A=M-1" \
        "M=-1" \
        //
        s"(CHECK_END_$suffix)"

    // set `D` to value from top of stack and `M` to value from 2nd top of stack
    private val popTwo = "@SP" \
      "AM=M-1" \ // decrement stack pointer and point to next value on stack
      "D=M" \ // save value from stack in D
      "A=A-1" // point to next value on stack

    // pop & push
    private val popOne = "@SP" \ "A=M-1"

  extension (s: String) inline def \(inline line: String) = s + "\n" + line
