package jackc

import scala.util.control.TailCalls.*
import scala.annotation.tailrec

import Grammar as G

object CompilationEngine:

  // def withFilter(fn: G => Boolean): ResultR[G] = ResultR:
  //   r.value.flatMap:
  //     case Left(err) => done(Left(err))
  //     case Right(g) => if fn(g) then done(Right(g)) else done(Left(Error.UnexpectedToken(s"Unexpected token $g")))

  // make XML ast out of tokens
  // page 183
  def compile(tokens: Iterator[Token]): Iterator[Grammar] =
    // compileClass(tokens)

    ???

  private def compileClass(tokens: Iterator[Token]): ResultT[G.Class] =
    tokens.take(3).toList match
      case Token.Keyword("class") :: Token.Identifier(name) :: Token.Symbol('{') :: Nil =>
        for
          classVarDecs <- compileClassVarDecs(tokens.drop(3))
          subroutines <- compileSubroutines(tokens.drop(3))
        yield G.Class(name, classVarDecs, subroutines)
      // TODO: handle `} -> tokens.drop(lengthToNextToken('}')).next == Token.Symbol('}')
      case ts => ResultT.error(Error.UnexpectedToken(s"Expected class declaration, got $ts"))

  private def compileClassVarDecs(tokens: Iterator[Token]): ResultT[Vector[G.ClassVarDec]] =
    @tailrec
    def go(tokens: Iterator[Token], acc: Vector[G.ClassVarDec]): ResultT[Vector[G.ClassVarDec]] =
      tokens.take(2).toList match
        case Nil => ResultT.of(acc)
        case Token.Keyword(kind: G.ClassVarKind) :: Token.Keyword(tpe) :: Nil =>
          val line = tokens.drop(1).takeWhile(_ != Token.Symbol(';'))
          val names = line
            .grouped(2)
            .withPartial(true)
            .toList
            .foldLeft(Vector.empty[String]): (acc, tokens) =>
              tokens.toList match
                case Token.Identifier(name) :: Token.Symbol(',') :: Nil => acc :+ name
                case Token.Identifier(name) :: Nil                      => acc :+ name
                case _                                                  => acc

          val length = line.length + 1 // + 1 for the semicolon

          go(tokens.drop(length), acc :+ G.ClassVarDec(kind, tpe, names))
        case other => ResultT.error(Error.UnexpectedToken(s"Expected class variable declaration, got $other"))
    go(tokens, Vector.empty)

  private def compileSubroutines(tokens: Iterator[Token]): ResultT[Vector[G.SubroutineDec]] =
    def go(tokens: Iterator[Token], acc: Vector[G.SubroutineDec]): ResultT[Vector[G.SubroutineDec]] =
      tokens.take(3).toList match
        case Nil => ResultT.of(acc)
        case Token.Keyword(kind: G.SubroutineKind) :: Token.Keyword(tpe) :: Token.Keyword(name) :: Nil =>
          for
            params <- compileParameterList(tokens.drop(3))
            body <- compileSubroutineBody(tokens.drop(3))
            length = 3 + params.params.length + 13 // TODO: 13 is a placeholder, handle body.length correctly
            subroutines <- go(tokens.drop(length), acc :+ G.SubroutineDec(kind, tpe, name, params, body))
          yield subroutines
        case other => ResultT.error(Error.UnexpectedToken(s"Expected subroutine declaration, got $other"))
    go(tokens, Vector.empty)

  private def compileParameterList(tokens: Iterator[Token]): ResultT[G.ParameterList] =
    ???

  private def compileSubroutineBody(tokens: Iterator[Token]): ResultT[G.SubroutineBody] =
    ???
