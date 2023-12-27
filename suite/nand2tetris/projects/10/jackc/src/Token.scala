package jackc

import scala.compiletime.*

enum Token:
  /** All possible keywords and reserved words in the Jack language
    */
  case Keyword(value: Tuple.Union[Token.PossibleKeywords])

  /** All possible symbols in the Jack language
    */
  case Symbol(value: Tuple.Union[Token.PossibleSymbols])

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
  val PossibleKeywords = constValueTuple[PossibleKeywords]
  // format: on

  val allPossibleKeywords = Token.PossibleKeywords.toList.asInstanceOf[List[Tuple.Union[Token.PossibleKeywords]]]

  type PossibleSymbols =
    ('{', '}', '(', ')', '[', ']', '.', ',', ';', '+', '-', '*', '/', '&', '|', '<', '>', '=', '~')
  val PossibleSymbols = constValueTuple[PossibleSymbols]

  val allPossibleSymbols = Token.PossibleSymbols.toList.asInstanceOf[List[Tuple.Union[Token.PossibleSymbols]]]

  // make regex that matches a single char from PossibleSymbols

  given XMLEncoder[Token] = XMLEncoder.from:
    case Token.Keyword(value) => XML.Element("keyword", XML.Text(value))
    case Token.Symbol(value) =>
      XML.Element(
        "symbol",
        XML.Text(
          value match
            case '&' => "&amp;"
            case '<' => "&lt;"
            case '>' => "&gt;"
            case n   => n.toString
        )
      )
    case Token.Identifier(value)  => XML.Element("identifier", XML.Text(value))
    case Token.IntConst(value)    => XML.Element("integerConstant", XML.Text(value.toString))
    case Token.StringConst(value) => XML.Element("stringConstant", XML.Text(value.toUpperCase.nn))

  given XMLEncoder[Vector[Token]] with
    def encode(value: Vector[Token]): XML =
      XML.Element("tokens", value.map(XMLEncoder[Token].encode)*)
