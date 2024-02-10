package jackc

import Grammar as G

/** Compiles Jack AST to Hack VM code
  */
object CompilationEngine:
  trait VMCode

  type Context = SymbolsTable

  def compile(ast: Grammar.Class): Either[Error, Vector[VMCode]] =
    val G.Class(_, varsDec, subroutines) = ast

    given Context = varsDec.foldLeft(SymbolsTable.empty.pushScope):
      case scope -> G.ClassVarDec(kind, tpe, names) =>
        names.foldLeft(scope): (scope, name) =>
          scope.addSymbol:
            kind match
              case "field"  => SymbolsTable.Entry.Field(name, 0)
              case "static" => SymbolsTable.Entry.Static(name, 0)

    def compileSubroutines(subroutines: Vector[G.SubroutineDec], acc: Vector[VMCode]): ResultT[Vector[VMCode]] =
      subroutines match
        case head +: tail => compileSubroutine(head).flatMap(h => compileSubroutines(tail, acc ++ h))
        case _            => ResultT.of(acc)

    compileSubroutines(subroutines.toVector, Vector.empty).run

  def compileSubroutine(ast: G.SubroutineDec)(using ctx: Context): ResultT[Vector[VMCode]] =
    val subroutineScope = ctx.pushScope

    // TODO: init subroutine scope and write vm code

    ???

// Symbols table usage
// - on every identifier declaration append it to the symbol table
// - on every identifier usage check if it's in the symbol table
// - on every class defs and subroutine defs create a new scope
// - on leaving a class def or subroutine def pop the scope
final case class SymbolsTable(scopes: List[Map[String, SymbolsTable.Entry]]) extends AnyVal:
  def pushScope: SymbolsTable = copy(scopes = Map.empty :: scopes)
  def popScope: SymbolsTable = copy(scopes = scopes.tail)

  def addSymbol(entry: SymbolsTable.Entry): SymbolsTable =
    copy(scopes = scopes match
      case head :: tail => (head + (entry.name -> entry)) :: tail
      case _            => Map(entry.name -> entry) :: Nil
    )
  def getSymbol(name: String): Option[SymbolsTable.Entry] =
    scopes.flatMap(_.get(name)).headOption

  // def lookupInCurrentScope(name: String): Option[SymbolTable.Entry] =
  //   scopes.headOption.flatMap(_.get(name))

  // def currentScope: Map[String, SymbolTable.Entry] = scopes.head

  // def currentScopeIndex: Int = scopes.length - 1

  // def reset: SymbolTable = SymbolTable.empty

object SymbolsTable:
  def empty = SymbolsTable(List.empty)

  sealed trait NamedEntry:
    def name: String

  enum Entry extends NamedEntry:
    case Field(name: String, index: Int)
    case Static(name: String, index: Int)
    case Var(name: String, index: Int)
    case Arg(name: String, index: Int)
    case Class(name: String)
    case Subroutine(name: String)
