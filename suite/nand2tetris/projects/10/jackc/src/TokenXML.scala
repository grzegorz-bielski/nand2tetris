package jackc

object TokenXML:
  given XMLEncoder[Token.Keyword] = XMLEncoder.from(t => XML.Element("keyword", XML.Text(t.value)))
  given XMLEncoder[Token.Symbol] = XMLEncoder.from: t =>
    XML.Element(
      "symbol",
      XML.Text(
        t.value match
          case '&' => "&amp;"
          case '<' => "&lt;"
          case '>' => "&gt;"
          case n   => n.toString
      )
    )
  given XMLEncoder[Token.Identifier] = XMLEncoder.from(t => XML.Element("identifier", XML.Text(t.value)))
  given XMLEncoder[Token.IntConst] = XMLEncoder.from(t => XML.Element("integerConstant", XML.Text(t.value.toString)))
  given XMLEncoder[Token.StringConst] = XMLEncoder.from(t => XML.Element("stringConstant", XML.Text(t.value)))

  given XMLEncoder[Token] = XMLEncoder.from:
    case t: Token.Keyword     => XMLEncoder[Token.Keyword].encode(t)
    case t: Token.Symbol      => XMLEncoder[Token.Symbol].encode(t)
    case t: Token.Identifier  => XMLEncoder[Token.Identifier].encode(t)
    case t: Token.IntConst    => XMLEncoder[Token.IntConst].encode(t)
    case t: Token.StringConst => XMLEncoder[Token.StringConst].encode(t)

  given XMLEncoder[Iterator[Token]] = XMLEncoder.from: value =>
    XML.Element("tokens", value.toSeq.map(XMLEncoder[Token].encode)*)