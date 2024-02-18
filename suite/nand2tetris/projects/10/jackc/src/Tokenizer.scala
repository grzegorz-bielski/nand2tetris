package jackc

import scala.util.chaining.*
import scala.annotation.tailrec
import scala.util.Using
import scala.io.Source
import scala.util.Try

import XMLEncoder.*

object Tokenizer:
  type Result = Either[Error, Iterator[Token]]

  def tokenize(iterator: Iterator[String]): Result =
    Try(unsafeTokenize(iterator)).asTokenizerErrors

  def unsafeTokenize(iterator: Iterator[String]): Iterator[Token] = iterator.stripComments.tokenize

  def tokenizeAt(srcPath: os.Path): Result = tokenizeAt(srcPath)(identity)

  // TODO: Move it to `test` scope as debug extension method. Tokenizer should not read files.
  def tokenizeAt[A](srcPath: os.Path)(fn: Iterator[Token] => A): Either[Error, A] =
    Using(Source.fromFile(srcPath.toIO)):
      _.getLines().pipe(unsafeTokenize).pipe(fn)
    .asTokenizerErrors

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
    /** Tokenize given file line by line, calling the `tokenizeAll` recursively on each line until all tokens are found
      * and `chunk` is exhausted.
      */
    def tokenize: Iterator[Token] =
      lines.toIterator
        .flatMap(TokensCollector.tokenizeAll)
        .flatten

  private object TokensCollector:
    def tokenizeAll(line: String): Iterator[Vector[Token]] =
      Iterator.unfold(line -> ""):
        case (chunk, prevChunk) =>
          if chunk.isEmpty then None
          else if chunk == prevChunk then
            println(s"Could not tokenize: $line")
            None
          else
            val (token, rest) = tokenizeKeywords(chunk)
              .pipeT(tokenizeIdentifiers)
              .pipeT(tokenizeSymbols)
              .pipeT(tokenizeIntConst)
              .pipeT(tokenizeStringConst)

            Some(token, rest -> chunk)

    private def tokenizeKeywords(chunk: String) =
      val builder = Vector.newBuilder[Token]

      val rest = Token.allPossibleKeywords.foldLeft(chunk.dropWhile(_.isWhitespace)): (acc, keyword) =>
        if acc.startsWith(keyword) then
          val next = acc.drop(keyword.length)
          if next.head.isLetterOrDigit then acc
          else
            builder.addOne(Token.Keyword(keyword))
            next
        else acc

      builder.result() -> rest

    private def tokenizeIdentifiers(chunk: String) =
      val id =
        if chunk.headOption.exists(_.isDigit)
        then ""
        else chunk.takeWhile(ch => ch.isLetterOrDigit || ch == '_')

      if id.nonEmpty then Vector(Token.Identifier(id)) -> chunk.drop(id.length)
      else Vector.empty -> chunk

    private def tokenizeSymbols(chunk: String) =
      val trimmed = chunk.dropWhile(_.isWhitespace)
      val symbols = trimmed.takeWhile(Token.allPossibleSymbols.contains)

      val tokens = symbols.toVector.map: symbol =>
        Token.Symbol(symbol.asInstanceOf[Tuple.Union[Token.PossibleSymbols]])

      tokens -> trimmed.drop(symbols.length)

    private def tokenizeIntConst(chunk: String) =
      val digits = chunk.takeWhile(_.isDigit)
      val followingChar = chunk.lift(digits.length + 1) // do not allow <digits><identifier>

      if digits.nonEmpty && !followingChar.exists(_.isLetter) then
        Vector(Token.IntConst(digits.toInt)) -> chunk.drop(digits.length)
      else Vector.empty -> chunk

    private def tokenizeStringConst(chunk: String) =
      if chunk.startsWith("\"") then
        val str = chunk.drop(1).takeWhile(_ != '"')
        Vector(Token.StringConst(str)) -> chunk.drop(str.length + 2)
      else Vector.empty -> chunk

  type TokenAcc = (Vector[Token], String)
  extension (underlying: TokenAcc)
    // like StateT flatMap
    def pipeT(fn: String => TokenAcc): TokenAcc =
      val res = fn(underlying._2)
      (underlying._1 ++ res._1, res._2)

  extension [A](underlying: Try[A])
    def asTokenizerErrors: Either[Error, A] =
      underlying.toEither.left.map(err => Error.TokenizerError(err.getMessage.nn))
