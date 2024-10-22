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
    localsCount: Int,
    argsCount: Int
):
  import SymbolsTable.*

  def pushScope: SymbolsTable = copy(scopes = Map.empty :: scopes)
  def popScope: SymbolsTable = copy(scopes = scopes.tail)

  def addSymbol(entry: SymbolsTable.Entry): SymbolsTable =
    def nextScopes(index: Int) = scopes match
      case head :: tail => (head + (entry.name -> (entry, index))) :: tail
      case _            => Map(entry.name -> (entry, index)) :: Nil

    entry match
      case _: Entry.Field  => copy(fieldsCount = fieldsCount + 1, scopes = nextScopes(fieldsCount))
      case _: Entry.Static => copy(staticsCount = staticsCount + 1, scopes = nextScopes(staticsCount))
      case _: Entry.Local  => copy(localsCount = localsCount + 1, scopes = nextScopes(localsCount))
      case _: Entry.Argument => copy(argsCount = argsCount + 1, scopes = nextScopes(argsCount))

  def getSymbol(name: String): Option[(SymbolsTable.Entry, Int)] =
    scopes.flatMap(_.get(name)).headOption

object SymbolsTable:
  def empty = SymbolsTable(List.empty, 0, 0, 0, 0)

  sealed trait NamedEntry:
    def name: String
    def `type`: String

  enum Entry extends NamedEntry:
    case Field(name: String, `type`: String)
    case Static(name: String, `type`: String)
    case Local(name: String, `type`: String)
    case Argument(name: String, `type`: String)
