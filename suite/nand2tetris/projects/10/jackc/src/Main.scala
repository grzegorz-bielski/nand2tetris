//> using scala 3.3.1
//> using toolkit latest
//> using option -Yexplicit-nulls

package jackc

import scala.util.chaining.*
import scala.annotation.tailrec
import geny.*

import XMLEncoder.*

// TODO:
// 1. Jack tokenizer
// 2. Jack compilation engine (syntax analyzer) without expressions and array-oriented statements
// 3. Jack engine with expressions
// 4. Jack engine with array-oriented statements

// scala-cli . -- $(pwd)/../ExpressionLessSquare/SquareGame.jack $(pwd)/../ExpressionLessSquare/SquareGameTC.xml
@main
def run(source: String, dest: String) =
  tokenizeToXML(source, dest)

def tokenizeToXML(source: String, dest: String) =
  val tokens = Tokenizer.tokenize(os.Path(source)).encode.toStringFormatted

  os.write.over(os.Path(dest), tokens)

enum Error:
  case SyntaxError(message: String)

object Tokenizer:
  import Comments.*

  /** Tokenize given file line by line, calling the `tokenizeAll` recursively on each line until all tokens are found
    * and `chunk` is exhausted.
    */
  def tokenize(srcPath: os.Path): Vector[Token] =
    val tokens = Vector.newBuilder[Token]

    def tokenizeKeywords(chunk: String) = Token.allPossibleKeywords.foldLeft(chunk.dropWhile(_.isWhitespace)):
      (acc, keyword) =>
        if acc.startsWith(keyword) then
          tokens.addOne(Token.Keyword(keyword))
          acc.drop(keyword.length)
        else acc

    def tokenizeIdentifiers(chunk: String) =
      val id =
        if chunk.headOption.exists(_.isDigit)
        then ""
        else chunk.takeWhile(ch => ch.isLetterOrDigit || ch == '_')

      if id.nonEmpty then
        tokens.addOne(Token.Identifier(id))
        chunk.drop(id.length)
      else chunk

    def tokenizeSymbols(chunk: String) =
      val trimmed = chunk.dropWhile(_.isWhitespace)
      val symbols = trimmed.takeWhile(Token.allPossibleSymbols.contains)

      symbols.foreach: symbol =>
        tokens.addOne(Token.Symbol(symbol.asInstanceOf[Tuple.Union[Token.PossibleSymbols]]))

      trimmed.drop(symbols.length)

    def tokenizeIntConst(chunk: String) =
      val digits = chunk.takeWhile(_.isDigit)

      if digits.nonEmpty then
        tokens.addOne(Token.IntConst(digits.toInt))
        chunk.drop(digits.length)
      else chunk

    def tokenizeStringConst(chunk: String) =
      if chunk.startsWith("\"") then
        val str = chunk.drop(1).takeWhile(_ != '"')
        tokens.addOne(Token.StringConst(str))
        chunk.drop(str.length + 2)
      else chunk

    def tokenizeAll(line: String): Unit | Error =
      @tailrec
      def go(chunk: String, prevChunk: String): Unit | Error =
        if chunk.isEmpty then ()
        else if chunk == prevChunk then Error.SyntaxError(s"Could not tokenize: $line")
        else
          go(
            tokenizeKeywords(chunk)
              .pipe(tokenizeIdentifiers)
              .pipe(tokenizeSymbols)
              .pipe(tokenizeIntConst)
              .pipe(tokenizeStringConst),
            chunk
          )

      go(line, "")

    os.read
      .lines(srcPath)
      .iterator
      .withRemovedComments
      .takeWhile:
        // little bit ugly - relies on side effects
        tokenizeAll(_) match
          case Error.SyntaxError(message) =>
            tokens.clear()
            println(message)
            false
          case _ => true
      .force

    tokens.result()

  object Comments:
    extension (iterator: Iterator[String])
      def withRemovedComments: LazyList[String] =
        def go(lines: LazyList[String], insideComment: Boolean): LazyList[String] =
          lines match
            // handle comment start
            case line #:: _ if !insideComment =>
              line.indexOf("//") match
                case -1 =>
                  line.indexOf("/*") match
                    // no comment, moving on
                    case -1 => line #:: go(lines.tail, insideComment = false)
                    // handle multi line comment
                    case i =>
                      val rest =
                        // check if there is closing tag on the same line
                        line.indexOf("*/") match
                          case -1 => go(lines.tail, insideComment = true)
                          case i  => go(line.drop(i + 2) #:: lines.tail, insideComment = false)

                      line.take(i) #:: rest
                // handle single line comment
                case i => go(line.take(i) #:: lines.tail, insideComment = false)

            // handle multiline comment end
            case line #:: _ if insideComment =>
              line.indexOf("*/") match
                case -1 => go(lines.tail, insideComment = true)
                case i  => line.drop(i + 2) #:: go(lines.tail, insideComment = false)

            case _ => LazyList.empty

        go(LazyList.from[String](iterator), insideComment = false)
