package jackc

import Grammar as G

object CompilationEngine:
    trait VMCode

    def compile(ast: Grammar.Class): Either[Error, VMCode] = 
        val classScope = SymbolsTable.empty.pushScope

        val G.Class(_, varsDec, subroutines) = ast
            
            // Context = SymbolsTable

            // TODO: use ResultT[(Context, VMCode)]

            // varsDec.foldLeft(classScope): 
            //     case scope -> G.ClassVarDec(kind, tpe, names) =>
            //         val nextScope = names.foldLeft(scope): (scope, name) => 
            //             scope.addSymbol:
            //                 kind match
            //                     case "field" => SymbolsTable.Entry.Field(name, 0)
            //                     case "static" => SymbolsTable.Entry.Static(name, 0)

            //         ???


            ???
                    
                    // entries.foreach(classScope.addSymbol)
        

// TODO:
// - on every identifier declaration append it to the symbol table
// - on every identifier usage check if it's in the symbol table
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
