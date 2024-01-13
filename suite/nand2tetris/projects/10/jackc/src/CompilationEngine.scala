package jackc

import scala.annotation.tailrec

import Grammar as G
import Token as T
import scala.util.Try

// refactoring ideas:
// keep state of consumed tokens somewhere - possibly in ResultT, or in a separate class (Iterator?)

object CompilationEngine:
  def compile(tokens: Iterator[Token]): Either[Error, Grammar.Class] =
    Try:
      val res = compileClass(LazyList.from(tokens)).run

      // println(res)

      res
    .toEither.left
      .map: err =>
        // println(err)
        Error.CompilationError(err.getMessage.nn)
      .joinRight

  private def compileClass(tokens: LazyList[Token]): ResultT[G.Class] =
    tokens match
      case Token.Keyword("class") #:: Token.Identifier(name) #:: Token.Symbol('{') #:: rest =>
        for
          (classVarLength, classVarDecs) <- compileClassVarDecs(rest)
          (subroutinesLength, subroutines) <- compileSubroutines(rest)
          length = classVarLength + subroutinesLength
          _ <- ResultT.matches(rest.drop(length).head):
            case T.Symbol('}') => ()
        yield G.Class(name, classVarDecs, subroutines)
      case other => unexpectedToken(other, "class declaration")

  private def compileClassVarDecs(tokens: LazyList[Token]): ResultT[(Int, Vector[G.ClassVarDec])] =
    @tailrec
    def go(tokens: LazyList[Token], acc: (Int, Vector[G.ClassVarDec])): ResultT[(Int, Vector[G.ClassVarDec])] =
      tokens match
        case T.Keyword(kind: G.ClassVarKind) #:: T.Keyword(tpe) #:: rest =>
          val (length, names) = collectVars(rest)
          go(rest.drop(length), (acc._1 + length, acc._2 :+ G.ClassVarDec(kind, tpe, names)))
        case _ => ResultT.of(acc)
    go(tokens, (0, Vector.empty))

  private def compileSubroutines(tokens: LazyList[Token]): ResultT[(Int, Vector[G.SubroutineDec])] =
    def go(tokens: LazyList[Token], acc: (Int, Vector[G.SubroutineDec])): ResultT[(Int, Vector[G.SubroutineDec])] =
      tokens match
        // format: off
        case T.Keyword(kind: G.SubroutineKind) #:: Token.Keyword(tpe) #:: T.Identifier(name) #:: T.Symbol('(') #:: rest =>
        // format: on
          for
            (paramsLength, params) <- compileParameterList(rest)
            (bodyLength, body) <- compileSubroutineBody(rest.drop(paramsLength))
            length = paramsLength + bodyLength + 4 // kind + tpe + name + `(`
            subroutines <- go(
              rest.drop(length),
              (acc._1 + length, acc._2 :+ G.SubroutineDec(kind, tpe, name, params, body))
            )
          yield subroutines
        case _ => ResultT.of(acc)
    go(tokens, (0, Vector.empty))

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
    tokens match
      case T.Symbol('{') #:: rest =>
        for
          (varDecsLength, varDecs) <- compileVarDecs(rest)
          (statementsLength, statements) <- compileStatements(rest.drop(varDecsLength))
          length = varDecsLength + statementsLength // + 1 for the `}`
          _ <- ResultT.matches(rest.drop(length)):
            case T.Symbol('}') #:: _ => ()
        yield length + 1 -> G.SubroutineBody(varDecs, statements)
      case other => unexpectedToken(other, "subroutine body")

  private def compileVarDecs(tokens: LazyList[Token]): ResultT[(Int, Vector[G.VarDec])] =
    def go(tokens: LazyList[Token], acc: (Int, Vector[G.VarDec])): ResultT[(Int, Vector[G.VarDec])] =
      inline def rec(rest: LazyList[Token], tpe: String) =
        val (varsLength, names) = collectVars(rest)
        val length = acc._1 + varsLength + 2 // var + tpe
        go(rest.drop(varsLength), (length, acc._2 :+ G.VarDec(tpe, names)))

      tokens match
        case T.Keyword("var") #:: T.Identifier(tpe) #:: rest => rec(rest, tpe)
        case T.Keyword("var") #:: T.Keyword(tpe) #:: rest    => rec(rest, tpe)
        case _                                               => ResultT.of(acc)
    go(tokens, (0, Vector.empty))

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

  private def compileStatements(originalTokens: LazyList[Token]): ResultT[(Int, G.Statements)] =
    def go(tokens: LazyList[Token], acc: (Int, Vector[G.Statement])): ResultT[(Int, Vector[G.Statement])] =
      inline def rec(result: (Int, G.Statement)) =
        go(tokens.drop(result._1), (acc._1 + result._1, acc._2 :+ result._2))

      tokens match
        case T.Keyword("let") #:: _    => compileLet(tokens).flatMap(rec(_))
        case T.Keyword("if") #:: _     => compileIf(tokens).flatMap(rec(_))
        case T.Keyword("while") #:: _  => compileWhile(tokens).flatMap(rec(_))
        case T.Keyword("do") #:: _     => compileDo(tokens).flatMap(rec(_))
        case T.Keyword("return") #:: _ => compileReturn(tokens).flatMap(rec(_))
        case _                         => ResultT.of(acc)
    go(originalTokens, (0, Vector.empty)).map(_ -> G.Statements(_))

  private def compileLet(tokens: LazyList[Token]): ResultT[(Int, G.Statement.Let)] =
    tokens match
      case T.Keyword("let") #:: T.Identifier(name) #:: T.Symbol('=') #:: rest =>
        compileExpression(rest.takeWhile(_ != T.Symbol(';'))).map: (exprLength, expression) =>
          val length = exprLength + 4 // let + , + = + ;
          length -> G.Statement.Let(name, None, expression)

      case T.Keyword("let") #:: T.Identifier(name) #:: T.Symbol('[') #:: rest =>
        val exprTokens = rest.takeWhile(_ != T.Symbol(']'))
        for
          (indexExprLength, indexExpr) <- compileExpression(exprTokens)
          afterIndexExpr = rest.drop(exprTokens.length + 1)
          _ <- ResultT.matches(afterIndexExpr):
            case T.Symbol('=') #:: _ => ()
          beforeAssignment = afterIndexExpr.drop(1).takeWhile(_ != T.Symbol(';'))
          (exprLength, expression) <- compileExpression(beforeAssignment)
          length = indexExprLength + exprLength + 6 // let + , + = + [ + ] + ;
        yield length -> G.Statement.Let(name, Some(indexExpr), expression)

  private def compileIf(tokens: LazyList[Token]): ResultT[(Int, G.Statement.If)] =
    tokens match
      case T.Keyword("if") #:: rest =>
        for
          (afterExpr, (ifExprLength, ifExpr)) <- compileBetween('(', ')', compileExpression)(rest)
          (afterStatement, (ifStmtLength, ifStatement)) <- compileBetween('{', '}', compileStatements)(afterExpr)
          res <- afterStatement match
            case T.Keyword("else") #:: afterElse =>
              compileBetween('{', '}', compileStatements)(afterElse).map:
                case (_, (elseStmtLength, elseStatement)) =>
                  val length = ifExprLength + ifStmtLength + elseStmtLength + 8 // if + ( + ) + { + } + else + { + }
                  length -> G.Statement.If(ifExpr, ifStatement, Some(elseStatement))
            case _ =>
              val length = ifExprLength + ifStmtLength + 5 // if + ( + ) + { + }
              ResultT.of(1 -> G.Statement.If(ifExpr, ifStatement, None))
        yield res
      case other => unexpectedToken(other, "if statement")

  private def compileWhile(tokens: LazyList[Token]): ResultT[(Int, G.Statement.While)] =
    tokens match
      case T.Keyword("while") #:: rest =>
        for
          (afterExpr, (exprLength, expr)) <- compileBetween('(', ')', compileExpression)(rest)
          (afterStmt, (stmtLength, stmt)) <- compileBetween('{', '}', compileStatements)(afterExpr)
          length = exprLength + stmtLength + 5 // while + ( + ) + { + }
        yield length -> G.Statement.While(expr, stmt)
      case other => unexpectedToken(other, "while statement")

  private def compileDo(tokens: LazyList[Token]): ResultT[(Int, G.Statement.Do)] =
    tokens match
      case T.Keyword("do") #:: rest =>
        compileSubroutineCallOrFail(rest.takeWhile(_ != T.Symbol(';'))).map: (callLength, expression) =>
          val length = callLength + 2 // do + ;
          length -> G.Statement.Do(expression)
      case other => unexpectedToken(other, "do statement")

  private def compileReturn(tokens: LazyList[Token]): ResultT[(Int, G.Statement.Return)] =
    tokens match
      case T.Keyword("return") #:: T.Symbol(';') #:: _ =>
        ResultT.of(2 -> G.Statement.Return(None))
      case T.Keyword("return") #:: rest =>
        val exprTokens = rest.takeWhile(_ != T.Symbol(';'))
        compileExpression(exprTokens).map: (exprLength, expression) =>
          val length = exprLength + 2 // return + ;
          length -> G.Statement.Return(Some(expression))
      case other => unexpectedToken(other, "return statement")

  private def compileExpression(tokens: LazyList[Token]): ResultT[(Int, G.Expression)] =
    def go(tokens: LazyList[Token], acc: (Int, Vector[(G.Op, G.Term)])): ResultT[(Int, Vector[(G.Op, G.Term)])] =
      tokens match
        case T.Symbol(op: G.Op) #:: rest =>
          for
            (termLength, term) <- compileTerm(rest)
            length = acc._1 + termLength + 1 // + 1 for the operator
            res <- go(rest.drop(termLength), (length, acc._2 :+ (op, term)))
          yield res
        case other => ResultT.of(acc)
    for
      (termLength, term) <- compileTerm(tokens)
      (exprLength, terms) <- go(tokens.drop(termLength), (0, Vector.empty))
      length = termLength + exprLength
    yield length -> G.Expression(term, terms*)

  private def compileTerm(tokens: LazyList[Token]): ResultT[(Int, G.Term)] =
    val pf: PartialFunction[LazyList[Token], ResultT[(Int, G.Term)]] =
      case T.IntConst(value) #:: _                  => ResultT.of(1 -> G.Term.IntConst(value))
      case T.StringConst(value) #:: _               => ResultT.of(1 -> G.Term.StringConst(value))
      case T.Keyword(name: G.KeywordConstant) #:: _ => ResultT.of(1 -> G.Term.KeywordConst(name))
      case T.Identifier(name) #:: T.Symbol('[') #:: rest =>
        val exprTokens = rest.takeWhile(_ != T.Symbol(']'))
        for
          (exprLength, expr) <- compileExpression(exprTokens)
          length = exprLength + 3 // + 3 for the name + [ + ]
        yield length -> G.Term.VarName(name, Some(expr))
      case T.Symbol('(') #:: rest =>
        for
          (exprLength, expr) <- compileExpression(rest)
          length = exprLength + 2 // + 2 for the ( + )
        yield length -> G.Term.Expr(expr)
      case T.Symbol(op: G.UnaryOp) #:: rest =>
        for
          (termLength, term) <- compileTerm(rest)
          length = termLength + 1 // + 1 for the operator
        yield length -> G.Term.Op(op, term)

    pf
      .orElse(compileSubroutineCall.andThen(_.map(_ -> G.Term.Call(_))))
      .orElse:
        case T.Identifier(name) #:: _ =>
          ResultT.of(1 -> G.Term.VarName(name, None))
      .applyOrElse(tokens, unexpectedToken(_, "term"))

  private val compileSubroutineCall: PartialFunction[LazyList[Token], ResultT[(Int, G.SubroutineCall)]] =
    case T.Identifier(receiver) #:: T.Symbol('.') #:: T.Identifier(name) #:: rest =>
      for
        (_, (exprLength, exprs)) <- compileBetween('(', ')', compileExpressionList)(rest)
        length = exprLength + 5 // + 5 for the receiver + . + name + ( + )
      yield length -> G.SubroutineCall(Some(receiver), name, exprs)
    case T.Identifier(name) #:: T.Symbol('(') #:: rest =>
      val exprLine = rest.takeWhile(_ != T.Symbol(')'))

      for
        (exprLength, exprs) <- compileExpressionList(exprLine)
        length = exprLength + 3 // + 3 for the name + ( + )
      yield length -> G.SubroutineCall(None, name, exprs)

  private val compileSubroutineCallOrFail = compileSubroutineCall.orElse(unexpectedToken(_, "subroutine call"))

  private def compileExpressionList(tokens: LazyList[Token]): ResultT[(Int, G.ExpressionList)] =
    if tokens.isEmpty then ResultT.of(0 -> G.ExpressionList(Vector.empty))
    else
      def go(tokens: LazyList[Token], acc: (Int, Vector[G.Expression])): ResultT[(Int, Vector[G.Expression])] =
        tokens match
          case T.Symbol(',') #:: rest =>
            for
              (exprLength, expr) <- compileExpression(rest)
              length = acc._1 + exprLength + 1 // + 1 for the comma
              res <- go(rest.drop(exprLength), (length, acc._2 :+ expr))
            yield res
          case other => ResultT.of(acc)

      for
        (exprLength, expr) <- compileExpression(tokens)
        (exprsLength, exprs) <- go(tokens.drop(exprLength), (0, Vector.empty))
        length = exprLength + exprsLength
      yield length -> G.ExpressionList(expr +: exprs)

  private def unexpectedToken[A](ll: LazyList[Token], expected: String): ResultT[A] =
    ResultT.error[A]:
      val msg =
        if ll.isEmpty then s"Unexpected end of input, expected $expected"
        else s"Expected $expected, got: ${ll.take(10).mkString(", ")} ..."

      Error.UnexpectedToken(msg)

  private def compileBetween[R](
      from: T.PossibleSymbolsU,
      to: T.PossibleSymbolsU,
      fn: LazyList[Token] => ResultT[R]
  )(tokens: LazyList[Token]): ResultT[(LazyList[Token], R)] =
    for
      (within, rest) <- between(from, to)(tokens)
      result <- fn(within)
    yield rest -> result

  private def between(
      from: T.PossibleSymbolsU,
      to: T.PossibleSymbolsU
  ): LazyList[Token] => ResultT[(LazyList[Token], LazyList[Token])] =
    _ match
      case T.Symbol(`from`) #:: rest =>
        val within = rest.takeWhile(_ != T.Symbol(to))
        val length = within.length + 1 // + 1 for the closing symbol

        ResultT.of(within -> rest.drop(length))
      case other =>
        unexpectedToken(other, s"$from...$to")