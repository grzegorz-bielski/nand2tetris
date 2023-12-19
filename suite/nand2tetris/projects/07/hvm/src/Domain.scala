package hvm

enum Command:
  case Arithmetic(name: String)
  case Push(a0: String, a1: Int)
  case Pop(a0: String, a1: Int)
  case Label(name: String)
  case Goto(label: String)
  case If(label: String)
  case Function(a0: String, a1: Int)
  case Return
  case Call(a0: String, a1: Int)

enum Error:
  case InvalidCommand(msg: String)