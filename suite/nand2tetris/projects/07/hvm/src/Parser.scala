package hvm


object Parser:
  type Result = Command | Error | Null

  // regex for three groups: command, arg1?, arg2?
  private val commandRegex = """^(\S+)(\s\S+)?(\s\S+)?$""".r

  private val commentRegex = """//.*"""

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

    line.replaceAll(commentRegex, "").nn.trim.nn match
      case line if line.isEmpty() => null
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
