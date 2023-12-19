package hvm

object HASMWriter:
  type Result = String | Error

  def writeCmd(cmd: Command, fileName: String, line: Int): Result =
    cmd match
      case Command.Arithmetic(name)   => IntegerArithmetic.from(name, fileName, line)
      case Command.Push(segment, pos) => StackArithmetic.from(StackArithmetic.Mode.Push, segment, pos, fileName)
      case Command.Pop(segment, pos)  => StackArithmetic.from(StackArithmetic.Mode.Pop, segment, pos, fileName)

      case Command.Label(name) => Branching.label(name)
      case Command.Goto(label) => Branching.goto(label)
      case Command.If(label)   => Branching.ifGoto(label)

      case _ => Error.InvalidCommand("Not implemented")

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

    private def pushToSegment(segment: String) =
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
    private val popFromStackToD = "@SP" \
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

    // TODO: use filename ?
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

  // BasicLoop, FibonacciSeries, and SimpleFunction do no need bootstrap code
  private object Branching:
    // labels and gotos are global

    def label(name: String) = s"($name)"

    def goto(label: String) = s"@$label" \ "0;JMP"

    def ifGoto(label: String) =
      "@SP" \ "AM=M-1" \ // pop one
        "D=M" \ s"@$label" \ "D;JNE" // jump to label if not equal to 0

  extension (s: String) inline def \(inline line: String) = s + "\n" + line
