package jackc

enum Error:
  case TokenizerError(message: String)
  case UnexpectedToken(message: String)
  case PredicateFailed(message: String)