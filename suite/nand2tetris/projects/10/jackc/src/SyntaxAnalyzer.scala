package jackc

import scala.annotation.tailrec

import Grammar as G
import Token as T
import scala.util.Try
import java.security.KeyStore.Entry

// refactoring ideas:
// keep state of consumed tokens somewhere - possibly in StateT, or in a separate class (Iterator?)

/**
  * Analyzes tokens stream and produces an AST.
  */
object SyntaxAnalyzer:
  def analyze(tokens: Iterator[Token]): Either[Error, Grammar.Class] =
    Try:
      analyzeClass(LazyList.from(tokens)).run
    .toEither.left
      .map: err =>
        Error.AnalyzerError(err.getMessage.nn)
      .joinRight

  private def analyzeClass(tokens: LazyList[Token]): ResultT[G.Class] =
    tokens match
      case T.Keyword("class") #:: T.Identifier(name) #:: T.Symbol('{') #:: rest =>
        for
          (classVarLength, classVarDecs) <- analyzeClassVarDecs(rest)
          (subroutinesLength, subroutines) <- analyzeSubroutines(rest.drop(classVarLength))
          length = classVarLength + subroutinesLength
          _ <- ResultT.matches("analyzeClass", rest.drop(length)):
            case T.Symbol('}') #:: _ => ()
        yield G.Class(name, classVarDecs, subroutines)
      case other => unexpectedToken(other, "class declaration")

  private def analyzeClassVarDecs(tokens: LazyList[Token]): ResultT[(Int, Vector[G.ClassVarDec])] =
    @tailrec
    def go(tokens: LazyList[Token], acc: (Int, Vector[G.ClassVarDec])): ResultT[(Int, Vector[G.ClassVarDec])] =
      tokens match
        case T.Keyword(kind: G.ClassVarKind) #:: (tpe: (T.Keyword | T.Identifier)) #:: rest =>
          val (length, names) = collectVars(rest)
          val nextLength = length + 2 // kind + tpe
          go(tokens.drop(nextLength), (acc._1 + nextLength, acc._2 :+ G.ClassVarDec(kind, typeValue(tpe), names)))
        case _ => ResultT.of(acc)
    go(tokens, (0, Vector.empty))

  private def analyzeSubroutines(tokens: LazyList[Token]): ResultT[(Int, Vector[G.SubroutineDec])] =
    def go(tokens: LazyList[Token], acc: (Int, Vector[G.SubroutineDec])): ResultT[(Int, Vector[G.SubroutineDec])] =
      tokens match
        // format: off
        case T.Keyword(kind: G.SubroutineKind) #:: (tpe: (T.Keyword | T.Identifier)) #:: T.Identifier(name) #:: T.Symbol('(') #:: rest =>
        // format: on
          for
            (paramsLength, params) <- analyzeParameterList(rest)
            (bodyLength, body) <- analyzeSubroutineBody(rest.drop(paramsLength))
            length = paramsLength + bodyLength + 4 // kind + tpe + name + `(`
            subroutines <- go(
              tokens.drop(length),
              (acc._1 + length, acc._2 :+ G.SubroutineDec(kind, typeValue(tpe), name, params, body))
            )
          yield subroutines
        case _ => ResultT.of(acc)
    go(tokens, (0, Vector.empty))

  private def analyzeParameterList(tokens: LazyList[Token]): ResultT[(Int, G.ParameterList)] =
    val line = tokens.takeWhile(_ != T.Symbol(')'))

    val params = line
      .grouped(3)
      .foldLeft(Vector.empty[G.Parameter]):
        case acc -> T.Keyword(tpe) #:: T.Identifier(name) #:: T.Symbol(',') #:: _ => acc :+ G.Parameter(tpe, name)
        case acc -> T.Keyword(tpe) #:: T.Identifier(name) #:: _                   => acc :+ G.Parameter(tpe, name)
        case acc -> _                                                             => acc

    val length = line.length + 1 // + 1 for the `)

    ResultT.of(length -> G.ParameterList(params))

  private def analyzeSubroutineBody(tokens: LazyList[Token]): ResultT[(Int, G.SubroutineBody)] =
    tokens match
      case T.Symbol('{') #:: rest =>
        for
          (varDecsLength, varDecs) <- analyzeVarDecs(rest)
          (statementsLength, statements) <- analyzeStatements(rest.drop(varDecsLength))
          length = varDecsLength + statementsLength
          _ <- ResultT.matches("analyzeSubroutineBody", rest.drop(length)):
            case T.Symbol('}') #:: _ => ()
          _length = length + 2 // + 2 for the `{` + `}`
        yield _length -> G.SubroutineBody(varDecs, statements)
      case other => unexpectedToken(other, "subroutine body")

  private def analyzeVarDecs(tokens: LazyList[Token]): ResultT[(Int, Vector[G.VarDec])] =
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

  private def analyzeStatements(originalTokens: LazyList[Token]): ResultT[(Int, G.Statements)] =
    def go(tokens: LazyList[Token], acc: (Int, Vector[G.Statement])): ResultT[(Int, Vector[G.Statement])] =
      inline def rec(result: (Int, G.Statement)) =
        go(tokens.drop(result._1), (acc._1 + result._1, acc._2 :+ result._2))

      tokens match
        case T.Keyword("let") #:: _    => analyzeLet(tokens).flatMap(rec(_))
        case T.Keyword("if") #:: _     => analyzeIf(tokens).flatMap(rec(_))
        case T.Keyword("while") #:: _  => analyzeWhile(tokens).flatMap(rec(_))
        case T.Keyword("do") #:: _     => analyzeDo(tokens).flatMap(rec(_))
        case T.Keyword("return") #:: _ => analyzeReturn(tokens).flatMap(rec(_))
        case _                         => ResultT.of(acc)
    go(originalTokens, (0, Vector.empty)).map(_ -> G.Statements(_))

  private def analyzeLet(tokens: LazyList[Token]): ResultT[(Int, G.Statement.Let)] =
    tokens match
      case T.Keyword("let") #:: T.Identifier(name) #:: T.Symbol('=') #:: rest =>
        analyzeExpression(rest.takeWhile(_ != T.Symbol(';'))).map: (exprLength, expression) =>
          val length = exprLength + 4 // let + , + = + ;
          length -> G.Statement.Let(name, None, expression)

      case T.Keyword("let") #:: T.Identifier(name) #:: T.Symbol('[') #:: rest =>
        val exprTokens = rest.takeWhile(_ != T.Symbol(']'))
        for
          (indexExprLength, indexExpr) <- analyzeExpression(exprTokens)
          afterIndexExpr = rest.drop(exprTokens.length + 1)
          _ <- ResultT.matches("analyzeLet", afterIndexExpr):
            case T.Symbol('=') #:: _ => ()
          beforeAssignment = afterIndexExpr.drop(1).takeWhile(_ != T.Symbol(';'))
          (exprLength, expression) <- analyzeExpression(beforeAssignment)
          length = indexExprLength + exprLength + 6 // let + , + = + [ + ] + ;
        yield length -> G.Statement.Let(name, Some(indexExpr), expression)

  private def analyzeIf(tokens: LazyList[Token]): ResultT[(Int, G.Statement.If)] =
    tokens match
      case T.Keyword("if") #:: rest =>
        for
          (ifExprLength, ifExpr) <- analyzeBetween('(', ')', analyzeExpression)(rest)
          afterExpr = rest.drop(ifExprLength)

          (ifStmtLength, ifStatement) <- analyzeBetween('{', '}', analyzeStatements)(afterExpr)
          afterStatement = afterExpr.drop(ifStmtLength)

          res <- afterStatement match
            case T.Keyword("else") #:: afterElse =>
              for
                (elseStmtLength, elseStatement) <- analyzeBetween('{', '}', analyzeStatements)(afterElse)
                length = ifExprLength + ifStmtLength + elseStmtLength + 2 // if + else
              yield length -> G.Statement.If(ifExpr, ifStatement, Some(elseStatement))
            case _ =>
              val length = ifExprLength + ifStmtLength + 1 // if
              ResultT.of(length -> G.Statement.If(ifExpr, ifStatement, None))
        yield res
      case other => unexpectedToken(other, "if statement")

  private def analyzeWhile(tokens: LazyList[Token]): ResultT[(Int, G.Statement.While)] =
    tokens match
      case T.Keyword("while") #:: rest =>
        for
          (exprLength, expr) <- analyzeBetween('(', ')', analyzeExpression)(rest)
          afterExpr = rest.drop(exprLength)
          (stmtLength, stmt) <- analyzeBetween('{', '}', analyzeStatements)(afterExpr)
          length = exprLength + stmtLength + 1 // while
        yield length -> G.Statement.While(expr, stmt)
      case other => unexpectedToken(other, "while statement")

  private def analyzeDo(tokens: LazyList[Token]): ResultT[(Int, G.Statement.Do)] =
    tokens match
      case T.Keyword("do") #:: rest =>
        analyzeSubroutineCallOrFail(rest.takeWhile(_ != T.Symbol(';'))).map: (callLength, expression) =>
          val length = callLength + 2 // do + ;
          length -> G.Statement.Do(expression)
      case other => unexpectedToken(other, "do statement")

  private def analyzeReturn(tokens: LazyList[Token]): ResultT[(Int, G.Statement.Return)] =
    tokens match
      case T.Keyword("return") #:: T.Symbol(';') #:: _ =>
        ResultT.of(2 -> G.Statement.Return(None))
      case T.Keyword("return") #:: rest =>
        val exprTokens = rest.takeWhile(_ != T.Symbol(';'))
        analyzeExpression(exprTokens).map: (exprLength, expression) =>
          val length = exprLength + 2 // return + ;
          length -> G.Statement.Return(Some(expression))
      case other => unexpectedToken(other, "return statement")

  private def analyzeExpression(tokens: LazyList[Token]): ResultT[(Int, G.Expression)] =
    def go(tokens: LazyList[Token], acc: (Int, Vector[(G.Op, G.Term)])): ResultT[(Int, Vector[(G.Op, G.Term)])] =
      tokens match
        case T.Symbol(op: G.Op) #:: rest =>
          for
            (termLength, term) <- analyzeTerm(rest)
            length = acc._1 + termLength + 1 // + 1 for the operator
            res <- go(rest.drop(termLength), (length, acc._2 :+ (op, term)))
          yield res
        case other => ResultT.of(acc)
    for
      (termLength, term) <- analyzeTerm(tokens)
      (exprLength, terms) <- go(tokens.drop(termLength), (0, Vector.empty))
      length = termLength + exprLength
    yield length -> G.Expression(term, terms*)

  private def analyzeTerm(tokens: LazyList[Token]): ResultT[(Int, G.Term)] =
    val pf: PartialFunction[LazyList[Token], ResultT[(Int, G.Term)]] =
      case T.IntConst(value) #:: _                  => ResultT.of(1 -> G.Term.IntConst(value))
      case T.StringConst(value) #:: _               => ResultT.of(1 -> G.Term.StringConst(value))
      case T.Keyword(name: G.KeywordConstant) #:: _ => ResultT.of(1 -> G.Term.KeywordConst(name))
      case T.Identifier(name) #:: T.Symbol('[') #:: rest =>
        val exprTokens = rest.takeWhile(_ != T.Symbol(']')) // TODO: analyzeBetween
        for
          (exprLength, expr) <- analyzeExpression(exprTokens)
          length = exprLength + 3 // + 3 for the name + [ + ]
        yield length -> G.Term.VarName(name, Some(expr))
      case T.Symbol('(') #:: rest =>
        for
          (exprLength, expr) <- analyzeExpression(rest) // TODO: analyzeBetween
          length = exprLength + 2 // + 2 for the ( + )
        yield length -> G.Term.Expr(expr)
      case T.Symbol(op: G.UnaryOp) #:: rest =>
        for
          (termLength, term) <- analyzeTerm(rest)
          length = termLength + 1 // + 1 for the operator
        yield length -> G.Term.Op(op, term)

    pf
      .orElse(analyzeSubroutineCall.andThen(_.map(_ -> G.Term.Call(_))))
      .orElse:
        case T.Identifier(name) #:: _ =>
          ResultT.of(1 -> G.Term.VarName(name, None))
      .applyOrElse(tokens, unexpectedToken(_, "term"))

  private val analyzeSubroutineCall: PartialFunction[LazyList[Token], ResultT[(Int, G.SubroutineCall)]] =
    case T.Identifier(receiver) #:: T.Symbol('.') #:: T.Identifier(name) #:: rest =>
      rest match
        case T.Symbol('(') #:: T.Symbol(')') #:: _ =>
          val length = 5 // + 5 for the receiver + . + name + ( + )
          ResultT.of:
            length -> G.SubroutineCall(Some(receiver), name, G.ExpressionList(Seq.empty))
        case _ =>
          for
            (exprLength, exprs) <- analyzeBetween('(', ')', analyzeExpressionList)(rest)
            length = exprLength + 3 // + 3 for the receiver + . + name
          yield length -> G.SubroutineCall(Some(receiver), name, exprs)
    case T.Identifier(name) #:: T.Symbol('(') #:: T.Symbol(')') #:: _ =>
      val length = 3 // + 3 for the name + ( + )
      ResultT.of:
        length -> G.SubroutineCall(None, name, G.ExpressionList(Seq.empty))

    case T.Identifier(name) #:: T.Symbol('(') #:: rest =>
      for
        (exprLength, exprs) <- analyzeBetween('(', ')', analyzeExpressionList)(T.Symbol('(') #:: rest)
        length = exprLength + 1 // + 1 for the name
      yield length -> G.SubroutineCall(None, name, exprs)

  private val analyzeSubroutineCallOrFail = analyzeSubroutineCall.orElse(unexpectedToken(_, "subroutine call"))

  private def analyzeExpressionList(tokens: LazyList[Token]): ResultT[(Int, G.ExpressionList)] =
    if tokens.isEmpty then ResultT.of(0 -> G.ExpressionList(Vector.empty))
    else
      def go(tokens: LazyList[Token], acc: (Int, Vector[G.Expression])): ResultT[(Int, Vector[G.Expression])] =
        tokens match
          case T.Symbol(',') #:: rest =>
            for
              (exprLength, expr) <- analyzeExpression(rest)
              length = acc._1 + exprLength + 1 // + 1 for the comma
              res <- go(rest.drop(exprLength), (length, acc._2 :+ expr))
            yield res
          case other => ResultT.of(acc)

      for
        (exprLength, expr) <- analyzeExpression(tokens)
        (exprsLength, exprs) <- go(tokens.drop(exprLength), (0, Vector.empty))
        length = exprLength + exprsLength
      yield length -> G.ExpressionList(expr +: exprs)

  private def unexpectedToken[A](ll: LazyList[Token], expected: String): ResultT[A] =
    ResultT.error[A]:
      val msg =
        if ll.isEmpty then s"Unexpected end of input, expected $expected"
        else s"Expected $expected, got: ${ll.take(10).mkString(", ")} ..."

      Error.UnexpectedToken(msg)

  private def analyzeBetween[R](
      from: T.PossibleSymbolsU,
      to: T.PossibleSymbolsU,
      fn: LazyList[Token] => ResultT[(Int, R)]
  )(tokens: LazyList[Token]) =
    for
      _ <- ResultT.matches(from.toString, tokens):
        case T.Symbol(`from`) #:: _ => ()
      (resLength, res) <- fn(tokens.drop(1))
      _ <- ResultT.matches(to.toString, tokens.drop(1 + resLength)):
        case T.Symbol(`to`) #:: _ => ()
      length = resLength + 2 // + 2 for the from + to
    yield length -> res

  private def typeValue(tpe: T.Keyword | T.Identifier) =
    tpe match
      case T.Keyword(value)    => value
      case T.Identifier(value) => value
