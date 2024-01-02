package jackc

import scala.annotation.tailrec

import Grammar as G
import Token as T

object CompilationEngine:
  def compile(tokens: Iterator[Token]): Iterator[Grammar] =
    val cls = compileClass(LazyList.from(tokens))

    ???

  private def compileClass(tokens: LazyList[Token]): ResultT[G.Class] =
    tokens match
      case Token.Keyword("class") #:: Token.Identifier(name) #:: Token.Symbol('{') #:: rest =>
        for
          classVarDecs <- compileClassVarDecs(rest)
          subroutines <- compileSubroutines(rest)
        yield G.Class(name, classVarDecs, subroutines)
      case other => unexpectedToken(other, "class declaration")

  private def compileClassVarDecs(tokens: LazyList[Token]): ResultT[Vector[G.ClassVarDec]] =
    @tailrec
    def go(tokens: LazyList[Token], acc: Vector[G.ClassVarDec]): ResultT[Vector[G.ClassVarDec]] =
      tokens match
        case ll if ll.isEmpty => ResultT.of(acc)
        case T.Keyword(kind: G.ClassVarKind) #:: T.Keyword(tpe) #:: rest =>
          val (length, names) = collectVars(rest)
          go(rest.drop(length), acc :+ G.ClassVarDec(kind, tpe, names))
        case other => unexpectedToken(other, "class variable declaration")
    go(tokens, Vector.empty)

  private def compileSubroutines(tokens: LazyList[Token]): ResultT[Vector[G.SubroutineDec]] =
    def go(tokens: LazyList[Token], acc: Vector[G.SubroutineDec]): ResultT[Vector[G.SubroutineDec]] =
      tokens match
        case ll if ll.isEmpty => ResultT.of(acc)
        case T.Keyword(kind: G.SubroutineKind) #:: Token.Keyword(tpe) #:: T.Keyword(name) #:: T.Symbol('(') #:: rest =>
          for
            (paramsLength, params) <- compileParameterList(rest)
            (bodyLength, body) <- compileSubroutineBody(rest)
            length = paramsLength + bodyLength
            subroutines <- go(rest.drop(length), acc :+ G.SubroutineDec(kind, tpe, name, params, body))
          yield subroutines
        case other => unexpectedToken(other, "subroutine declaration")
    go(tokens, Vector.empty)

  private def compileParameterList(tokens: LazyList[Token]): ResultT[(Int, G.ParameterList)] =
    val line = tokens.takeWhile(_ != T.Symbol(')'))

    val params = line
      .grouped(3)
      .foldLeft(Vector.empty[G.Parameter]):
        case acc -> T.Keyword(tpe) #:: T.Identifier(name) #:: T.Symbol(',') #:: _ => acc :+ G.Parameter(tpe, name)
        case acc -> T.Keyword(tpe) #:: T.Identifier(name) #:: _                   => acc :+ G.Parameter(tpe, name)
        case acc -> _                                                             => acc

    val length = line.length + 1 // + 1 for the `)

    ResultT.of(length -> G.ParameterList(params))

  private def compileSubroutineBody(tokens: LazyList[Token]): ResultT[(Int, G.SubroutineBody)] =
    val lines = tokens.takeWhile(_ != T.Symbol('}'))

    lines match
      case T.Symbol('{') #:: rest =>
        for
          varDecs <- compileVarDecs(rest)
          statements <- compileStatements(rest)
          length = lines.length + 1 // + 1 for the `}`
        yield length -> G.SubroutineBody(varDecs, statements)
      case other => unexpectedToken(other, "subroutine body")

  private def compileVarDecs(tokens: LazyList[Token]): ResultT[Vector[G.VarDec]] =
    @tailrec
    def go(tokens: LazyList[Token], acc: Vector[G.VarDec]): ResultT[Vector[G.VarDec]] =
      tokens match
        case ll if ll.isEmpty => ResultT.of(acc)
        case T.Keyword("var") #:: T.Keyword(tpe) #:: rest =>
          val (length, names) = collectVars(rest)
          go(rest.drop(length), acc :+ G.VarDec(tpe, names))
        case other => unexpectedToken(other, "variable declaration")
    go(tokens, Vector.empty)

  private def collectVars(tokens: LazyList[Token]): (Int, Vector[String]) =
    val line = tokens.takeWhile(_ != T.Symbol(';'))
    val length = line.length + 1 // + 1 for the semicolon
    val names = line
      .grouped(2)
      .foldLeft(Vector.empty[String]):
        case acc -> T.Identifier(name) #:: T.Symbol(',') #:: _ => acc :+ name
        case acc -> T.Identifier(name) #:: _                   => acc :+ name
        case acc -> _                                          => acc

    length -> names

  private def unexpectedToken[A](ll: LazyList[Token], expected: String): ResultT[A] =
    ResultT.error[A](Error.UnexpectedToken(s"Expected $expected, got ${ll.take(4)}..."))

  private def compileStatements(tokens: LazyList[Token]): ResultT[G.Statements] =
    def go(tokens: LazyList[Token], acc: Vector[G.Statement]): ResultT[Vector[G.Statement]] =
      inline def rec(rest: LazyList[Token], result: (Int, G.Statement)) = go(rest.drop(result._1), acc :+ result._2)
      tokens match
        case ll if ll.isEmpty             => ResultT.of(acc)
        case T.Keyword("let") #:: rest    => compileLet(rest).flatMap(rec(rest, _))
        case T.Keyword("if") #:: rest     => compileIf(rest).flatMap(rec(rest, _))
        case T.Keyword("while") #:: rest  => compileWhile(rest).flatMap(rec(rest, _))
        case T.Keyword("do") #:: rest     => compileDo(rest).flatMap(rec(rest, _))
        case T.Keyword("return") #:: rest => compileReturn(rest).flatMap(rec(rest, _))
        case other                        => unexpectedToken(other, "statement")
    go(tokens, Vector.empty).map(G.Statements.apply)

  private def compileLet(tokens: LazyList[Token]): ResultT[(Int, G.Statement.Let)] = 
    tokens match 
      case T.Keyword("let") #:: T.Identifier(name) #:: rest => ???
        // for 
        //   (length, index) <- compileExpression(rest)
        //   _ <- expect(rest.drop(length), T.Symbol(']'))
        //   _ <- expect(rest.drop(length + 1), T.Symbol('='))
        //   (length2, value) <- compileExpression(rest.drop(length + 2))
        // yield length + length2 + 3 -> G.Statement.Let(name, Some(index), value)
    ???


  private def compileIf(tokens: LazyList[Token]): ResultT[(Int, G.Statement.If)] = ???

  private def compileWhile(tokens: LazyList[Token]): ResultT[(Int, G.Statement.While)] = ???

  private def compileDo(tokens: LazyList[Token]): ResultT[(Int, G.Statement.Do)] = ???

  private def compileReturn(tokens: LazyList[Token]): ResultT[(Int, G.Statement.Return)] = ???

  private def compileExpression(tokens: LazyList[Token]): ResultT[G.Expression] = ???

  private def compileTerm(tokens: LazyList[Token]): ResultT[G.Term] = ???

  private def compileExpressionList(tokens: LazyList[Token]): ResultT[G.ExpressionList] = ???

  private def compileSubroutineCall(tokens: LazyList[Token]): ResultT[G.SubroutineCall] = ???

  private def compileOp(tokens: LazyList[Token]): ResultT[G.Op] = ???

  private def compileUnaryOp(tokens: LazyList[Token]): ResultT[G.UnaryOp] = ???

  private def compileKeywordConstant(tokens: LazyList[Token]): ResultT[G.KeywordConstant] = ???
