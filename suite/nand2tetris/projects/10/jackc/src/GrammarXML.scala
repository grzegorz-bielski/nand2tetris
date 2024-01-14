package jackc

import Token as T
import Grammar as G
import XMLEncoder.*
import TokenXML.given

/** XML Representation of the Jack grammar for the Syntax Analyzer tests
  */
object GrammarXML:

  given XMLEncoder[G.ExpressionList] = XMLEncoder.from: g =>
    XML.Element("expressionList", g.expressions.map(XMLEncoder[G.Expression].encode)*)

  given XMLEncoder[G.Term] = XMLEncoder.from: g =>
    val body = g match
      case G.Term.IntConst(value)     => Seq(T.IntConst(value).encode)
      case G.Term.StringConst(value)  => Seq(T.StringConst(value).encode)
      case G.Term.KeywordConst(value) => Seq(T.Keyword(value).encode)
      case G.Term.VarName(name, index) =>
        T.Identifier(name).encode +: index.toSeq.flatMap: i =>
          Seq(T.Symbol('[').encode, XMLEncoder[G.Expression].encode(i), T.Symbol(']').encode)
      case G.Term.Call(call)  => fromSubroutineCall(call)
      case G.Term.Expr(value) => Seq(T.Symbol('(').encode, XMLEncoder[G.Expression].encode(value), T.Symbol(')').encode)
      case G.Term.Op(op, term) => Seq(T.Symbol(op).encode, XMLEncoder[G.Term].encode(term))

    XML.Element("term", body*)

  given XMLEncoder[G.Expression] = XMLEncoder.from: g =>
    val body = XMLEncoder[G.Term].encode(g.term) +: g.rest.flatMap:
      case (op: G.Op, term) => Seq(T.Symbol(op).encode, XMLEncoder[G.Term].encode(term))

    XML.Element("expression", body*)

  given XMLEncoder[G.ParameterList] = XMLEncoder.from: g =>
    val body = g.params.headOption.toSeq.flatMap(param) ++
      g.params.drop(1).flatMap(p => T.Symbol(',').encode +: param(p))

    XML.Element("parameterList", body*)

  given XMLEncoder[G.ClassVarDec] = XMLEncoder.from: g =>
    val body = Seq(
      T.Keyword(g.kind).encode,
      paramType(g.`type`),
      T.Identifier(g.names.head).encode
    ) ++ g.names.drop(1).flatMap(n => Seq(T.Symbol(',').encode, T.Identifier(n).encode)) :+
      T.Symbol(';').encode

    XML.Element("classVarDec", body*)

  given XMLEncoder[G.Statement] = XMLEncoder.from:
    case s: G.Statement.Let =>
      val body = Seq(
        T.Keyword("let").encode,
        T.Identifier(s.name).encode
      ) ++ s.index.toSeq
        .flatMap(i => Seq(T.Symbol('[').encode, XMLEncoder[G.Expression].encode(i), T.Symbol(']').encode)) ++
        Seq(T.Symbol('=').encode, XMLEncoder[G.Expression].encode(s.value), T.Symbol(';').encode)

      XML.Element("letStatement", body*)
    case s: G.Statement.If =>
      val body = Seq(
        T.Keyword("if").encode,
        T.Symbol('(').encode,
        XMLEncoder[G.Expression].encode(s.condition),
        T.Symbol(')').encode,
        T.Symbol('{').encode
      ) :+ XMLEncoder[G.Statements].encode(s.onTrue) :+
        T.Symbol('}').encode

      val _body = s.onFalse.fold(body): f =>
        body :+ T.Keyword("else").encode :+ T.Symbol('{').encode :+
          XMLEncoder[G.Statements].encode(f) :+ T.Symbol('}').encode

      XML.Element("ifStatement", _body*)
    case s: G.Statement.While =>
      val body = Seq(
        T.Keyword("while").encode,
        T.Symbol('(').encode,
        XMLEncoder[G.Expression].encode(s.condition),
        T.Symbol(')').encode,
        T.Symbol('{').encode
      ) :+ XMLEncoder[G.Statements].encode(s.body) :+
        T.Symbol('}').encode

      XML.Element("whileStatement", body*)
    case s: G.Statement.Do =>
      val body = T.Keyword("do").encode +: fromSubroutineCall(s.call) :+ T.Symbol(';').encode

      XML.Element("doStatement", body*)
    case s: G.Statement.Return =>
      val body =
        T.Keyword("return").encode +: s.value.toSeq
          .flatMap(v => Seq(XMLEncoder[G.Expression].encode(v))) :+ T.Symbol(';').encode

      XML.Element("returnStatement", body*)

  given XMLEncoder[G.Statements] = XMLEncoder.from: g =>
    XML.Element("statements", g.statements.map(XMLEncoder[G.Statement].encode)*)

  given XMLEncoder[G.VarDec] = XMLEncoder.from: g =>
    val body = Seq(T.Keyword("var").encode, paramType(g.`type`), T.Identifier(g.names.head).encode) ++
      g.names.drop(1).flatMap(n => Seq(T.Symbol(',').encode, T.Identifier(n).encode)) :+
      T.Symbol(';').encode

    XML.Element("varDec", body*)

  given XMLEncoder[G.SubroutineBody] = XMLEncoder.from: g =>
    val body = T.Symbol('{').encode +:
      g.varDecs.map(XMLEncoder[G.VarDec].encode) :+
      XMLEncoder[G.Statements].encode(g.statements) :+
      T.Symbol('}').encode

    XML.Element("subroutineBody", body*)

  given XMLEncoder[G.SubroutineDec] = XMLEncoder.from: g =>
    val body = Seq(
      T.Keyword(g.kind).encode,
      paramType(g.`type`),
      T.Identifier(g.name).encode,
      T.Symbol('(').encode
    ) :+ XMLEncoder[G.ParameterList].encode(g.params) :+
      T.Symbol(')').encode :+
      XMLEncoder[G.SubroutineBody].encode(g.body)

    XML.Element("subroutineDec", body*)

  given XMLEncoder[G.Class] = XMLEncoder.from: g =>
    val body = Seq(
      T.Keyword("class").encode,
      T.Identifier(g.name).encode,
      T.Symbol('{').encode
    ) ++ g.classVarDecs.map(XMLEncoder[G.ClassVarDec].encode) ++
      g.subroutines.map(XMLEncoder[G.SubroutineDec].encode) :+
      T.Symbol('}').encode

    XML.Element("class", body*)

  private def fromSubroutineCall(call: G.SubroutineCall): Seq[XML] =
    val fnCall =
      Seq(T.Identifier(call.name).encode, T.Symbol('(').encode) :+
        XMLEncoder[G.ExpressionList].encode(call.args) :+
        T.Symbol(')').encode

    call.receiver.fold(fnCall): r =>
      Seq(T.Identifier(r).encode, T.Symbol('.').encode) ++ fnCall

  // could be a keyword or identifier
  private def param(p: G.Parameter) =
    Seq(paramType(p.`type`), T.Identifier(p.name).encode)

  private def paramType(`type`: String) =
    val name = if T.allPossibleKeywords.contains(`type`) then "keyword" else "identifier"

    XML.Element(name, XML.Text(`type`))
