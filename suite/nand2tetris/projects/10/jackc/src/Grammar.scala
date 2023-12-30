package jackc

/** Jack grammar representation
  */
sealed trait Grammar
object Grammar:
  // `class` className `{` classVarDec* subroutineDec* `}`
  final case class Class(name: String, classVarDecs: Seq[ClassVarDec], subroutines: Seq[SubroutineDec]) extends Grammar
  // (`static` | `field`) type varName (, varName)* `;`
  final case class ClassVarDec(kind: "static" | "field", `type`: String, names: Seq[String]) extends Grammar
  // (`constructor` | `function` | `method`) (`void` | type) subroutineName `(`parameterList`)` subroutineBody
  final case class SubroutineDec(
      kind: "constructor" | "function" | "method",
      `type`: String,
      name: String,
      params: ParameterList,
      body: SubroutineBody
  ) extends Grammar
  // ((type varName) (`,` type varName)*)?
  final case class ParameterList(params: Seq[Parameter]) extends Grammar
  // `{` varDec* statements `}`
  final case class SubroutineBody(varDecs: Seq[VarDec], statements: Seq[Statement]) extends Grammar
  // `var` type varName (`,` varName)* `;`
  final case class VarDec(`type`: String, names: Seq[String]) extends Grammar
  // statement*
  final case class Statements(statements: Seq[Statement]) extends Grammar
  // term (op term)*
  final case class Expression(term: Term, rest: (Op, Term)*) extends Grammar
  // (expression(`,` expression)*)?
  final case class ExpressionList(expressions: Seq[Expression]) extends Grammar

  enum Statement extends Grammar:
    // `let` varName (`[` expression `]`)? `=` expression `;`
    case Let(name: String, index: Option[Expression], value: Expression)
    // `if` `(` expression `)` `{` statements `}` (`else` `{` statements `}`)?
    case If(condition: Expression, onTrue: Statements, onFalse: Option[Statements])
    // `while` `(` expression `)` `{` statements `}`
    case While(condition: Expression, body: Statements)
    // `do` subroutineCall `;`
    case Do(call: SubroutineCall)
    // `return` expression? `;`
    case Return(value: Option[Expression])

    // integerConstant | stringConstant | keywordConstant | varName | varName`[` expression `]` | subroutineCall | `(` expression `)` | unaryOp term
  enum Term extends Grammar:
    case IntConst(value: Int)
    case StringConst(value: String)
    case KeywordConst(value: "true" | "false" | "null" | "this")
    case VarName(name: String, index: Option[Expression])
    case Call(call: SubroutineCall)
    case Expr(value: Expression)
    case Op(op: UnaryOp, term: Term)

  // subroutineName `(` expressionList `)` | (className | varName) `.` subroutineName `(` expressionList `)`
  final case class SubroutineCall(receiver: Option[String], name: String, args: ExpressionList) extends Grammar

  final case class Parameter(`type`: String, name: String) extends Grammar

  type Op = "+" | "-" | "*" | "/" | "&" | "|" | "<" | ">" | "="
  type UnaryOp = "-" | "~"
  type KeywordConstant = "true" | "false" | "null" | "this"
