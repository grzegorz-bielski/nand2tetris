package jackc

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
