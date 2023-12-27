//> using scala 3.3.1
//> using toolkit latest
//> using option -Yexplicit-nulls

package jackc

import scala.util.chaining.*
import scala.annotation.tailrec
import geny.*

// TODO:
// 1. Jack tokenizer
// 2. Jack compilation engine (syntax analyzer) without expressions and array-oriented statements
// 3. Jack engine with expressions
// 4. Jack engine with array-oriented statements

// scala-cli . -- $(pwd)/../ArrayTest/Main.jack
@main
def run(path: String) =
  println(Tokenizer.tokenizeAll(os.Path(path)))

enum Error:
  case SyntaxError(message: String)

object Tokenizer:
  import Comments.*

  def tokenizeAll(srcPath: os.Path): Vector[Token] =
    val tokens = Vector.newBuilder[Token]

    def tokenizeKeywords(chunk: String) = Token.allPossibleKeywords.foldLeft(chunk.dropWhile(_.isWhitespace)):
      (acc, keyword) =>
        if acc.startsWith(keyword) then
          tokens.addOne(Token.Keyword(keyword))
          acc.drop(keyword.length + 1)
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

    def tokenizeAll(chunk: String): Unit =
      if chunk.isEmpty then ()
      else
        tokenizeAll:
          tokenizeKeywords(chunk)
            .pipe(tokenizeIdentifiers)
            .pipe(tokenizeSymbols)
            .pipe(tokenizeIntConst)
            .pipe(tokenizeStringConst)

    os.read
      .lines(srcPath)
      .iterator
      .withRemovedComments
      .foreach(tokenizeAll)

    tokens.result()

  object Comments:
    val commentPatterns = List[(String, Option[String])](
      "/**" -> Some("*/"),
      "/*" -> Some("*/"),
      "//" -> None
    )

    extension (iterator: Iterator[String])
      def withRemovedComments: Iterator[String] =
        val builder = Iterator.newBuilder[String]

        def next(chunk: String, until: Option[String]) =
          until.fold(iterator.nextOption()): u =>
            chunk.indexOf(u) match
              // search for EOC on the next line
              case -1 =>
                val iter = iterator.dropWhile(!_.contains(u))
                iter.next()
                iter.nextOption()
              // found EOC on the same line, create artificial chunk
              case i => Some(chunk.drop(i + u.length))

        @tailrec
        def traverse(remaining: String, nextChunk: Option[String]): Unit =
          nextChunk match
            case None => ()
            case Some(chunk) =>
              @tailrec
              def go(patterns: List[(String, Option[String])]): Unit =
                patterns match
                  case Nil =>
                    builder.addOne(remaining + chunk)
                    traverse("", iterator.nextOption()) // TODO: make it actually lazy
                  case (pattern, until) :: remainingPatterns =>
                    chunk.indexOf(pattern) match
                      // check other patterns for comments
                      case -1 => go(remainingPatterns)
                      // comment found
                      case i => traverse(chunk.take(i), next(chunk, until))
              go(commentPatterns)

        traverse("", iterator.nextOption())
        builder.result()
