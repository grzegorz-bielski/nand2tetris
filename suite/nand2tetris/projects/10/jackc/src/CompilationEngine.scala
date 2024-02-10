package jackc

import Grammar as G
import scala.util.chaining.*

/** Compiles Jack AST to Hack VM code
  */
object CompilationEngine:
  opaque type VMCode = String
  object VMCode:
    def apply(code: String): VMCode = code

  extension (sc: StringContext) def hvm(args: Any*): VMCode = sc.s(args: _*).pipe(VMCode.apply)

  // type Context = SymbolsTable
  final case class Context(scope: SymbolsTable, className: String)

  def compile(ast: Grammar.Class): Either[Error, Vector[VMCode]] =
    val G.Class(name, varsDec, subroutines) = ast

    val classScope = varsDec.foldLeft(SymbolsTable.empty.pushScope):
      case scope -> G.ClassVarDec(kind, tpe, names) =>
        names.foldLeft(scope): (scope, name) =>
          scope.addSymbol:
            kind match
              case "field"  => SymbolsTable.Entry.Field(name)
              case "static" => SymbolsTable.Entry.Static(name)

    given Context = Context(classScope, name)

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
        if kind == "method" then scope.addSymbol(SymbolsTable.Entry.Local("this")) -> 1 else scope -> 0
      .pipe:
        paramsList.params.foldLeft(_):
          case (scope, nVars) -> G.Parameter(_, name) => scope.addSymbol(SymbolsTable.Entry.Local(name)) -> (nVars + 1)
      .pipe:
        varsDec.foldLeft(_):
          case (scope, nVars) -> G.VarDec(_, names) =>
            names.view.map(SymbolsTable.Entry.Local(_)).foldLeft(scope)(_ addSymbol _) -> (nVars + names.size)

    Vector(hvm"${ctx.className}.${name} $nVars").pipe: code =>
      kind match
        case "method" =>
          for
            metCode <- ResultT.of:
              code ++ Vector(
                // aligns the virtual memory segment `this` with the base address of the object on which the method was called
                hvm"push argument 0",
                hvm"pop pointer 0"
              )
            stmtsCode <- compileStatements(statements)
          yield metCode ++ stmtsCode

        case "constructor" =>
          for
            consCode <- ResultT.of:
              code ++ Vector(
                // allocates memory of `fieldsCount` 16-bit words and aligns the virtual memory segment `this` with the base address of newly allocated block
                hvm"push constant ${ctx.scope.fieldsCount}",
                hvm"call Memory.alloc 1" // built-in OS function that allocates heap memory
              )
            stmtsCode <- compileStatements(statements)
          yield consCode ++
            stmtsCode :+
            hvm"pop pointer 0" // returns to the caller base address of the newly created object

        case "function" => compileStatements(statements)

  private def compileStatements(statements: G.Statements)(using ctx: Context): ResultT[Vector[VMCode]] =
    statements.statements.foldLeft(ResultT.of(Vector.empty)):
      case (acc, statement) =>
        for
          acc <- acc
          code <- compileStatement(statement)
        yield acc ++ code

  private def compileStatement(statement: G.Statement)(using ctx: Context): ResultT[Vector[VMCode]] = ???

// Symbols table usage
// - on every identifier declaration append it to the symbol table
// - on every identifier usage check if it's in the symbol table
// - on every class defs and subroutine defs create a new scope
// - on leaving a class def or subroutine def pop the scope
final case class SymbolsTable(
    scopes: List[Map[String, (SymbolsTable.Entry, Int)]],
    fieldsCount: Int,
    staticsCount: Int,
    localsCount: Int
):
  import SymbolsTable.*

  def pushScope: SymbolsTable = copy(scopes = Map.empty :: scopes)
  def popScope: SymbolsTable = copy(scopes = scopes.tail)

  def addSymbol(entry: SymbolsTable.Entry): SymbolsTable =
    def nextScopes(index: Int) = scopes match
      case head :: tail => (head + (entry.name -> (entry, index))) :: tail
      case _            => Map(entry.name -> (entry, index)) :: Nil

    entry match
      case Entry.Field(_)  => copy(fieldsCount = fieldsCount + 1, scopes = nextScopes(fieldsCount))
      case Entry.Static(_) => copy(staticsCount = staticsCount + 1, scopes = nextScopes(staticsCount))
      case Entry.Local(_)  => copy(localsCount = localsCount + 1, scopes = nextScopes(localsCount))

  def getSymbol(name: String): Option[(SymbolsTable.Entry, Int)] =
    scopes.flatMap(_.get(name)).headOption

object SymbolsTable:
  def empty = SymbolsTable(List.empty, 0, 0, 0)

  sealed trait NamedEntry:
    def name: String

  enum Entry extends NamedEntry:
    case Field(name: String)
    case Static(name: String)
    case Local(name: String)
    // case Arg(name: String)
    // case Class(name: String)
    // case Subroutine(name: String)
