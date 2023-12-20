package hvm

import scala.util.chaining.*

/** HVM backend for the hack computer platform
  */
object HASMWriter:
  type Result = String | Error

  def writeCmd(cmd: Command, fileName: String, line: Int, pf: Option[ParentFunction]): Result =
    cmd match
      case Command.Arithmetic(name)   => IntegerArithmetic.from(name, fileName, line)
      case Command.Push(segment, pos) => StackArithmetic.from(StackArithmetic.Mode.Push, segment, pos, fileName)
      case Command.Pop(segment, pos)  => StackArithmetic.from(StackArithmetic.Mode.Pop, segment, pos, fileName)

      case Command.Label(label) => Branching.label(Branching.LabelId(fileName, pf.map(_.name), label))
      case Command.Goto(label)  => Branching.goto(Branching.LabelId(fileName, pf.map(_.name), label))
      case Command.If(label)    => Branching.ifGoto(Branching.LabelId(fileName, pf.map(_.name), label))

      case Command.Function(name, nVars) => Functions.declaration(fileName, name, nVars)
      case Command.Call(name, nArgs) => Functions.ReturnAddress.of(fileName, pf).pipe(Functions.call(_, name, nArgs))
      case Command.Return            => Functions.`return`

  def writeBootstrap: String = Bootstrap.code

  // push segment i -> pushes the value of segment[i] onto the stack
  // pop segment i -> pops the value at the top of the stack and stores it in segment[i]
  // @SP holds the address of the RAM entry just after the topmost of the stack
  private object StackArithmetic:
    enum Mode:
      case Push, Pop

    def from(mode: Mode, segment: String, pos: Int, fileName: String): Result =
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

    private def static(mode: Mode, pos: Int, fileName: String) = segmentAt(mode, s"@$fileName.$pos")

    private def constant(mode: Mode, value: Int): Result =
      mode match
        case Mode.Push => s"@$value" \ "D=A" \ pushToStackToD
        case Mode.Pop  => Error.InvalidCommand("Cannot pop to constant")

    private def segmentAt(mode: Mode, name: String, offset: Int): Result =
      segmentAt(mode, segmentOffset(name, offset))

    private def segmentAt(mode: Mode, name: String): Result =
      (
        mode match
          case Mode.Push => pushToSegment
          case Mode.Pop  => popFromSegment
      ).apply(name)

    def pushToSegment(segment: String) =
      segment \
        "D=M" \ // load value from segment
        pushToStackToD

    private def popFromSegment(segment: String) = segment \
      "D=A" \ // save segment address in D
      "@R13" \
      "M=D" \ // save segment address in R13
      popFromStackToD \
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

      // sets `D` to value from top of stack
    // SP--;  D = RAM[SP]
    val popFromStackToD = "@SP" \
      "AM=M-1" \ // decrement stack pointer and point to next value on stack
      "D=M" // set value from stack to `D`

    // requires setting `D` for a value to push
    // RAM[SP] = D; SP++
    private val pushToStackToD = "@SP" \
      "AM=M+1" \ // increment stack pointer and point to next empty cell
      "A=A-1" \ // point to the previous top of stack
      "M=D" // set top of stack to value from `D`

  private object IntegerArithmetic:
    def from(cmd: String, fileName: String, line: Int): Result =
      cmd match
        case "add" => popTwoAndPush \ "M=D+M"
        case "sub" => popTwoAndPush \ "M=M-D"
        case "neg" => popAndPush \ "M=-M"
        case "and" => popTwoAndPush \ "M=D&M"
        case "or"  => popTwoAndPush \ "M=D|M"
        case "not" => popAndPush \ "M=!M"
        case "eq"  => cmp("JEQ", fileName, line)
        case "gt"  => cmp("JGT", fileName, line)
        case "lt"  => cmp("JLT", fileName, line)

    private def cmp(code: String, fileName: String, line: Int) =
      val suffix = s"${fileName}_${line}"

      popTwoAndPush \
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

    // pop two & push one
    // set `D` to value from top of stack and `M` to value from 2nd top of stack
    private val popTwoAndPush = "@SP" \
      "AM=M-1" \ // decrement stack pointer and point to next value on stack
      "D=M" \ // save value from stack in D
      "A=A-1" // point to next value on stack

    // pop & push one
    private val popAndPush = "@SP" \ "A=M-1"

  private object Branching:
    final case class LabelId(fileName: String, functionName: Option[String], labelName: String):
      def value = s"$fileName.${functionName.getOrElse("")}$$$labelName"

    def label(id: LabelId) = s"(${id.value})"

    def goto(id: LabelId) = s"@${id.value}" \ "0;JMP"

    def ifGoto(id: LabelId) =
      "@SP" \
        "AM=M-1" \ // pop one
        "D=M" \
        s"@${id.value}" \
        "D;JNE" // jump to label if not equal to 0

  private object Functions:
    import StackArithmetic.*

    def declaration(fileName: String, name: String, nVars: Int) =
      s"($fileName.$name)" \
        s"@SP" \ // set `A` to stack pointer
        "A=M" \ // point to stack pointer
        (0 until 5).map(_ => s"M=0" \ "A=A+1").mkString("\n") \ // set `M` to fn() for each variable
        "D=A" \ // set `D` to stack pointer + nVars
        "@SP" \
        "M=D" // set stack pointer to `D`

    enum ReturnAddress:
      case Root

      /** @param fileName
        *   file name in which parent function is declared
        * @param parentFunction
        *   name of the parent function
        * @param i
        *   call number within parent function
        */
      case Nested(fileName: String, parentFunction: String, i: Int)

      override def toString = this match
        case Root                                => "root.ret.0"
        case Nested(fileName, parentFunction, i) => s"$fileName.$parentFunction.ret.$i"

    object ReturnAddress:
      def of(fileName: String, pf: Option[ParentFunction]) =
        pf.fold(Root)(pf => Nested(fileName, pf.name, pf.callCount))

    // assumes function declaration has been called and it exists
    def call(returnAddress: ReturnAddress, functionName: String, nArgs: Int) =
      pushToSegment(s"@$returnAddress") \ // save return address
        pushToSegment("@LCL") \
        pushToSegment("@ARG") \
        pushToSegment("@THIS") \
        pushToSegment("@THAT") \
        // ARG = SP - 5 - nArgs
        s"@SP" \
        "D=M" \ // set `D` to stack pointer
        s"@5" \
        "D=D-A" \ // set `D` to stack pointer - 5
        s"@$nArgs" \
        "D=D-A" \ // set `D` to stack pointer - 5 - nArgs
        "@ARG" \
        "M=D" \ // set ARG to stack pointer - 5 - nArgs
        // LCL = SP
        "@SP" \
        "D=M" \ // set `D` to stack pointer
        "@LCL" \
        "M=D" \ // set LCL to stack pointer
        // goto function
        s"@$functionName" \
        "0;JMP" \
        s"($returnAddress)"

    lazy val `return` =
      val frame = "@R13" // temp variable

      def fromFrameAt(offset: Int) =
        frame \ "D=M" \ s"@$offset" \ "A=D-A"

      def fromFrameToAt(variable: String, offset: Int) =
        fromFrameAt(offset) \ "D=M" \ s"@$variable" \ "M=D"

      // frame = LCL
      "@LCL" \
        "D=M" \
        frame \
        "M=D" \
        // Copy the return value onto argument 0 (*ARG = pop()), and sets SP to point to the address just following it. (SP = ARG + 1)
        // This effectively frees the global stack area below the new value of SP.
        // Thus, when the caller resumes its execution, it sees the return value at the top of its working stack.
        // ---
        // *ARG = pop()
        "@SP" \
        "A=M-1" \
        "D=M" \
        "@ARG" \
        "A=M" \ // *ARG = D, change the value ARG points to
        "M=D" \
        // SP = ARG + 1
        "@ARG" \
        "D=M+1" \
        "@SP" \
        "M=D" \
        // THAT = *(frame - 1)
        fromFrameToAt("THAT", 1) \
        // THIS = *(frame - 2)
        fromFrameToAt("THIS", 2) \
        // ARG = *(frame - 3)
        fromFrameToAt("ARG", 3) \
        // LCL = *(frame - 4)
        fromFrameToAt("LCL", 4) \
        // goto *(frame - 5) // go to return address from stack saved by `call`
        fromFrameAt(5) \
        "A=M" \
        "0;JMP"
  private object Bootstrap:
    import Functions.*

    lazy val code =
      // maps stack on the host from 256 address onward
      "@256" \
        "D=A" \
        "@SP" \
        "M=D" \
        call(ReturnAddress.Root, "Sys.init", 0) // in turn calls `Main.main`

  extension (s: String) inline def \(inline line: String) = s + "\n" + line
