package jackc

import scala.compiletime.*

/** A terminal element of the Jack language grammar */
enum Token:
  /** All possible keywords and reserved words in the Jack language
    */
  case Keyword(value: Token.PossibleKeywordsU)

  /** All possible symbols in the Jack language
    */
  case Symbol(value: Token.PossibleSymbolsU)

  /** A sequence of letters, digits, underscore (_) that does not start with a digit
    */
  case Identifier(value: String)

  /** A decimal number in the range 0..32767 (unsigned 16-bit integer)
    */
  case IntConst(value: Int)

  /** A sequence of Unicode characters not including double quotes (") or newline
    */
  case StringConst(value: String)

object Token:
  // format: off
  type PossibleKeywords = 
    ("class", "constructor", "function", "method", "field", "static", "var", "int", "char", "boolean", "void", "true", "false", "null", "this", "let", "do", "if", "else", "while", "return")
  type PossibleKeywordsU = Tuple.Union[PossibleKeywords]
  val PossibleKeywords = constValueTuple[PossibleKeywords]
  // format: on

  val allPossibleKeywords = Token.PossibleKeywords.toList.asInstanceOf[List[PossibleKeywordsU]]

  type PossibleSymbols =
    ('{', '}', '(', ')', '[', ']', '.', ',', ';', '+', '-', '*', '/', '&', '|', '<', '>', '=', '~')
  type PossibleSymbolsU = Tuple.Union[PossibleSymbols]
  val PossibleSymbols = constValueTuple[PossibleSymbols]

  val allPossibleSymbols = Token.PossibleSymbols.toList.asInstanceOf[List[PossibleSymbolsU]]
