package jackc

import scala.util.chaining.*
import scala.annotation.tailrec
import scala.util.Using

import XMLEncoder.*

object Tokenizer:
  type Result = Either[Error, Vector[Token]]

  /** Tokenize given file line by line, calling the `tokenizeAll` recursively on each line until all tokens are found
    * and `chunk` is exhausted.
    */
  def tokenize(srcPath: os.Path): Result =
    Using(scala.io.Source.fromFile(srcPath.toIO)):
      _.getLines().stripComments.tokenize
    .toEither.left.map(err => Error.TokenizerError(err.getMessage.nn)).joinRight

  extension (iterator: Iterator[String])
    def stripComments: LazyList[String] =
      def go(lines: LazyList[String], insideComment: Boolean): LazyList[String] =
        lines match
          // handle comment start
          case line #:: _ if !insideComment =>
            line.indexOf("//") match
              case -1 =>
                line.indexOf("/*") match
                  // no comment, moving on
                  case -1 => line #:: go(lines.tail, insideComment = false)
                  // handle multiline comment
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

  extension (lines: LazyList[String])
    def tokenize: Result =
      val collector = TokensCollector()

      @tailrec
      def go(lines: LazyList[String]): Result =
        lines match
          case line #:: rest =>
            collector.tokenizeAll(line) match
              case err @ Error.TokenizerError(message) => Left(err)
              case _                                   => go(rest)
          case _ => Right(collector.collect())

      go(lines)

  private final class TokensCollector:
    private val tokens = Vector.newBuilder[Token]

    def collect() = tokens.result()

    def tokenizeAll(line: String): Unit | Error =
      @tailrec
      def go(chunk: String, prevChunk: String): Unit | Error =
        if chunk.isEmpty then ()
        else if chunk == prevChunk then Error.TokenizerError(s"Could not tokenize: $line")
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

    private def tokenizeKeywords(chunk: String) = Token.allPossibleKeywords.foldLeft(chunk.dropWhile(_.isWhitespace)):
      (acc, keyword) =>
        if acc.startsWith(keyword) then
          tokens.addOne(Token.Keyword(keyword))
          acc.drop(keyword.length)
        else acc

    private def tokenizeIdentifiers(chunk: String) =
      val id =
        if chunk.headOption.exists(_.isDigit)
        then ""
        else chunk.takeWhile(ch => ch.isLetterOrDigit || ch == '_')

      if id.nonEmpty then
        tokens.addOne(Token.Identifier(id))
        chunk.drop(id.length)
      else chunk

    private def tokenizeSymbols(chunk: String) =
      val trimmed = chunk.dropWhile(_.isWhitespace)
      val symbols = trimmed.takeWhile(Token.allPossibleSymbols.contains)

      symbols.foreach: symbol =>
        tokens.addOne(Token.Symbol(symbol.asInstanceOf[Tuple.Union[Token.PossibleSymbols]]))

      trimmed.drop(symbols.length)

    private def tokenizeIntConst(chunk: String) =
      val digits = chunk.takeWhile(_.isDigit)
      val followingChar = chunk.lift(digits.length + 1) // do not allow <digits><identifier>

      if digits.nonEmpty && !followingChar.exists(_.isLetter) then
        tokens.addOne(Token.IntConst(digits.toInt))
        chunk.drop(digits.length)
      else chunk

    private def tokenizeStringConst(chunk: String) =
      if chunk.startsWith("\"") then
        val str = chunk.drop(1).takeWhile(_ != '"')
        tokens.addOne(Token.StringConst(str))
        chunk.drop(str.length + 2)
      else chunk