package jackc

/** Jack grammar representation
  */
sealed trait Grammar
object Grammar:
  // `class` className `{` classVarDec* subroutineDec* `}`
  final case class Class(name: String, classVarDecs: Seq[ClassVarDec], subroutines: Seq[SubroutineDec]) extends Grammar
  // (`static` | `field`) type varName (, varName)* `;`
  final case class ClassVarDec(kind: ClassVarKind, `type`: String, names: Seq[String]) extends Grammar
  type ClassVarKind = "static" | "field"
  // (`constructor` | `function` | `method`) (`void` | type) subroutineName `(`parameterList`)` subroutineBody
  final case class SubroutineDec(
      kind: SubroutineKind,
      `type`: String,
      name: String,
      params: ParameterList,
      body: SubroutineBody
  ) extends Grammar
  type SubroutineKind = "constructor" | "function" | "method"
  // ((type varName) (`,` type varName)*)?
  final case class ParameterList(params: Seq[Parameter]) extends Grammar
  // `{` varDec* statements `}`
  final case class SubroutineBody(varDecs: Seq[VarDec], statements: Statements) extends Grammar
  // `var` type varName (`,` varName)* `;`
  final case class VarDec(`type`: String, names: Seq[String]) extends Grammar
  // statement*
  final case class Statements(statements: Seq[Statement]) extends Grammar
  // term (op term)*
  final case class Expression(term: Term, rest: (Op, Term)*) extends Grammar
  // (expression(`,` expression)*)?
  final case class ExpressionList(expressions: Seq[Expression]) extends Grammar

  // not `enum` because in constructors we want to infer child type like `Let` or `If` instead of `Statement`
  trait Statement extends Grammar
  object Statement:
    // `let` varName (`[` expression `]`)? `=` expression `;`
    final case class Let(name: String, index: Option[Expression], value: Expression) extends Statement
    // `if` `(` expression `)` `{` statements `}` (`else` `{` statements `}`)?
    final case class If(condition: Expression, onTrue: Statements, onFalse: Option[Statements]) extends Statement
    // `while` `(` expression `)` `{` statements `}`
    final case class While(condition: Expression, body: Statements) extends Statement
    // `do` subroutineCall `;`
    final case class Do(call: SubroutineCall) extends Statement
    // `return` expression? `;`
    final case class Return(value: Option[Expression]) extends Statement

  enum Term extends Grammar:
    // integerConstant
    case IntConst(value: Int)
    // stringConstant
    case StringConst(value: String)
    // keywordConstant
    case KeywordConst(value: KeywordConstant)
    // varName | varName`[` expression `]`
    case VarName(name: String, index: Option[Expression])
    // subroutineCall
    case Call(call: SubroutineCall)
    // `(` expression `)`
    case Expr(value: Expression)
    // unaryOp term
    case Op(op: UnaryOp, term: Term)

  // subroutineName `(` expressionList `)` |
  // (className | varName) `.` subroutineName `(` expressionList `)`
  final case class SubroutineCall(receiver: Option[String], name: String, args: ExpressionList) extends Grammar

  final case class Parameter(`type`: String, name: String) extends Grammar

  type Op = '+' | '-' | '*' | '/' | '&' | '|' | '<' | '>' | '='
  type UnaryOp = '-' | '~'
  type KeywordConstant = "true" | "false" | "null" | "this"
