package jackc

import Grammar as G
import scala.util.chaining.*

import SymbolsTable.Entry.*
import java.util.concurrent.atomic.AtomicInteger
import os.makeDir.all
import com.sourcegraph.semanticdb_javac.Result

/** Compiles Jack AST to Hack VM code
  */
object CompilationEngine:
  opaque type VMCode = String
  object VMCode:
    def apply(code: String): VMCode = code

  extension (sc: StringContext) def hvm(args: Any*): VMCode = sc.s(args*).pipe(VMCode.apply)

  // `mutableRunningLabelCounter` is the only peace of mutable state in the whole program
  // it's used to generate unique labels for `if-goto` and `goto` vm commands
  // alternatively, we could use a `UUID`, or store the counter in the `Context` and pass it around using `StateT`
  private final case class Context(scope: SymbolsTable, className: String, mutableRunningLabelCounter: AtomicInteger)

  def compile(ast: Grammar.Class): Either[Error, Vector[VMCode]] =
    val G.Class(name, varsDec, subroutines) = ast

    val classScope = varsDec.foldLeft(SymbolsTable.empty.pushScope):
      case scope -> G.ClassVarDec(kind, tpe, names) =>
        names.foldLeft(scope): (scope, name) =>
          scope.addSymbol:
            kind match
              case "field"  => Field(name, tpe)
              case "static" => Static(name, tpe)

    given Context = Context(classScope, name, AtomicInteger(0))

    def compileSubroutines(subroutines: List[G.SubroutineDec], acc: Vector[VMCode]): ResultT[Vector[VMCode]] =
      subroutines match
        case head :: tail => compileSubroutine(head).flatMap(h => compileSubroutines(tail, acc ++ h))
        case _            => ResultT.of(acc)

    compileSubroutines(subroutines.toList, Vector.empty).run

  private def compileSubroutine(ast: G.SubroutineDec)(using ctx: Context): ResultT[Vector[VMCode]] =
    val G.SubroutineDec(kind, tpe, name, paramsList, body) = ast
    val G.SubroutineBody(varsDec, statements) = body

    val (subroutineScope, nVars) = ctx.scope.pushScope
      .pipe: scope =>
        if kind == "method" then scope.addSymbol(SymbolsTable.Entry.Local("this", tpe)) -> 1 else scope -> 0
      .pipe:
        paramsList.params.foldLeft(_):
          case (scope, nVars) -> G.Parameter(_, name) =>
            scope.addSymbol(SymbolsTable.Entry.Local(name, tpe)) -> (nVars + 1)
      .pipe:
        varsDec.foldLeft(_):
          case (scope, nVars) -> G.VarDec(_, names) =>
            names.view.map(SymbolsTable.Entry.Local(_, tpe)).foldLeft(scope)(_ addSymbol _) -> (nVars + names.size)

    Vector(hvm"${ctx.className}.${name} $nVars").pipe: code =>
      kind match
        case "method" =>
          for
            metCode <- ResultT.of:
              // aligns the virtual memory segment `this` with the
              // base address of the object on which the method was called
              code :+ hvm"push argument 0" :+ hvm"pop pointer 0"
            stmtsCode <- compileStatements(statements)
          yield metCode ++ stmtsCode

        case "constructor" =>
          for
            consCode <- ResultT.of:
              // allocates memory of `fieldsCount` 16-bit words
              // and aligns the virtual memory segment `this` with the base address of newly allocated block
              code :+ hvm"push constant ${ctx.scope.fieldsCount}" :+ hvm"call Memory.alloc 1" // built-in OS function that allocates heap memory
            stmtsCode <- compileStatements(statements)
          yield consCode ++ stmtsCode :+ hvm"pop pointer 0" // returns to the caller base address of the newly created object

        case "function" => compileStatements(statements)

  private def compileStatements(statements: G.Statements)(using ctx: Context): ResultT[Vector[VMCode]] =
    statements.statements.foldLeft(ResultT.of(Vector.empty)):
      case (acc, statement) =>
        for
          acc <- acc
          code <- compileStatement(statement)
        yield acc ++ code

  private def compileStatement(statement: G.Statement)(using ctx: Context): ResultT[Vector[VMCode]] =
    statement match
      case G.Statement.Let(name, None, value) =>
        for
          exprValue <- compileExpression(value)
          popValue <- ctx.scope.symbolCmdOrErr(name, "pop")
        yield exprValue :+ popValue

      // if there is index / offset - assume it's an array
      case G.Statement.Let(name, Some(offsetExpr), value) =>
        for
          pushBaseAddr <- ctx.scope.symbolCmdOrErr(name, "push")
          pushOffset <- compileExpression(offsetExpr)
          pushValue <- compileExpression(value)
        yield (pushBaseAddr +: pushOffset :+ hvm"add") ++ // push `baseAddr + offset` to the top of the stack
          pushValue :+ // push exor value to the top of the stack
          hvm"pop temp 0" :+ // safe `value` from expr to set in temp 0
          hvm"pop pointer 1" :+ // set the base address `that` to the address of the array[baseAddr + offset]
          hvm"push temp 0" :+ // push the `value` back to the stack
          hvm"pop that 0" // set the `that` at 0 to the `value`

      case G.Statement.If(condition, onTrue, onFalse) =>
        val suffix = ctx.mutableRunningLabelCounter.getAndIncrement

        for
          conditionCode <- compileExpression(condition)
          onTrueCode <- compileStatements(onTrue)
          onFalseCode <- onFalse.fold(ResultT.of(Vector.empty))(compileStatements)
        yield (
          conditionCode :+
            hvm"not" :+
            hvm"if-goto IF_TRUE$suffix" :+
            hvm"goto IF_FALSE$suffix" :+
            hvm"label IF_TRUE$suffix"
        ) ++ (
          if onFalse.isEmpty
          then onTrueCode :+ hvm"label IF_FALSE$suffix"
          else
            (onTrueCode :+ hvm"goto IF_END$suffix") ++ (hvm"label IF_FALSE$suffix" +: onFalseCode :+ hvm"label IF_END$suffix")
        )

      case G.Statement.While(condition, body) =>
        val suffix = ctx.mutableRunningLabelCounter.getAndIncrement

        for
          conditionCode <- compileExpression(condition)
          bodyCode <- compileStatements(body)
        yield (hvm"label WHILE_EXP$suffix" +: conditionCode :+ hvm"not" :+ hvm"if-goto WHILE_END$suffix") ++
          (bodyCode :+ hvm"goto WHILE_EXP$suffix" :+ hvm"label WHILE_END$suffix")

      case G.Statement.Do(call) =>
        // get rid of the expression, call for its side effects only
        compileSubroutineCall(call).map(_ :+ hvm"pop temp 0")

      case G.Statement.Return(value) =>
        value
          .fold(ResultT.of(Vector(hvm"push constant 0")))(compileExpression) // void return type
          .map(_ :+ hvm"return")

  private def compileExpression(expression: G.Expression)(using ctx: Context): ResultT[Vector[VMCode]] =
    expression.rest.foldLeft(compileTerm(expression.term)):
      case (acc, (op, term)) =>
        for
          acc <- acc
          term <- compileTerm(term)
        yield acc ++ term :+ opCode(op)

  private def compileTerm(term: G.Term)(using ctx: Context): ResultT[Vector[VMCode]] =
    term match
      case G.Term.IntConst(value) => ResultT.of(Vector(hvm"push constant $value"))
      case G.Term.StringConst(value) =>
        ResultT.of:
          hvm"push constant ${value.length}" +:
            hvm"call String.new 1" +:
            value.zipWithIndex.toVector.flatMap:
              case (char, i) => Vector(hvm"push constant ${char.toInt}", hvm"call String.appendChar 2")

      case G.Term.KeywordConst(const) =>
        ResultT.of:
          const match
            case "true"           => Vector(hvm"push constant 0", hvm"not") // push -1 to the top of the stack
            case "false" | "null" => Vector(hvm"push constant 0")
            case "this"           => Vector(hvm"push pointer 0")

      case G.Term.VarName(name, maybeIndex) =>
        maybeIndex match
          case None => ctx.scope.symbolCmdOrErr(name, "push").map(Vector(_))
          case Some(index) =>
            for
              pushBaseAddr <- ctx.scope.symbolCmdOrErr(name, "push")
              pushIndex <- compileExpression(index)
            yield pushBaseAddr +: pushIndex :+ hvm"add" :+ hvm"pop pointer 1" :+ hvm"push that 0"

      case G.Term.Expr(expr) => compileExpression(expr)

      case G.Term.Op(op, term) => compileTerm(term).map(_ :+ opCode(op))

      case G.Term.Call(call) => compileSubroutineCall(call)

  private def opCode(op: Char): VMCode =
    op match
      case '+' => hvm"add"
      case '-' => hvm"sub"
      case '*' => hvm"call Math.multiply 2"
      case '/' => hvm"call Math.divide 2"
      case '&' => hvm"and"
      case '|' => hvm"or"
      case '<' => hvm"lt"
      case '>' => hvm"gt"
      case '=' => hvm"eq"
      case '~' => hvm"not"

  private def compileSubroutineCall(call: G.SubroutineCall)(using ctx: Context): ResultT[Vector[VMCode]] =
    val G.SubroutineCall(maybeReceiver, subroutineName, args) = call
    val nArgs = args.expressions.size

    val pushReceiver = ResultT.of:
      maybeReceiver match
        case None | Some("this") => Vector(hvm"push pointer 0")
        case Some(receiver)      => ctx.scope.symbolCmd(receiver, "push").toVector

    // if there is a receiver then take the type from the symbol table or assume it's an external class
    // if there is no receiver then assume it's a method of the current class
    val typeName = maybeReceiver
      .map(r => ctx.scope.getSymbol(r).fold(r)(_._1.`type`))
      .getOrElse(ctx.className)

    for
      pushReceiver <- pushReceiver
      pushArgs <- args.expressions.foldLeft(ResultT.of(Vector.empty[VMCode])):
        case (acc, expr) =>
          for
            acc <- acc
            expr <- compileExpression(expr)
          yield acc ++ expr
    yield pushReceiver ++ pushArgs :+ hvm"call $typeName.$subroutineName $nArgs"

  extension (underlying: SymbolsTable)
    def symbolCmd(name: String, cmd: String): Option[VMCode] =
      underlying
        .getSymbol(name)
        .collect:
          // not found identifiers should be assumed subroutine names or class name
          case (_: Field, index)  => hvm"$cmd this $index"
          case (_: Static, index) => hvm"$cmd static $index"
          case (_: Local, index)  => hvm"$cmd local $index"

    def symbolCmdOrErr(name: String, cmd: String): ResultT[VMCode] =
      symbolCmd(name, cmd).fold(ResultT.error(Error.CompilationError(s"Symbol $name not found")))(ResultT.of(_))
