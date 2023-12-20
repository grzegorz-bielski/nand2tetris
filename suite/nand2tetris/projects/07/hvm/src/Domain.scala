package hvm

enum Command:
  case Arithmetic(name: String)
  case Push(a0: String, a1: Int)
  case Pop(a0: String, a1: Int)
  case Label(name: String)
  case Goto(label: String)
  case If(label: String)
  case Function(name: String, nVars: Int)
  case Return
  case Call(function: String, nArgs: Int)

enum Error:
  case InvalidCommand(msg: String)