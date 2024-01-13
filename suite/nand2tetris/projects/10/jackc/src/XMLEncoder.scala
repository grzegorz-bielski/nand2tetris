package jackc

enum XML:
  case Element(name: String, value: XML*)
  case Text(value: String)

  final case class FormattingOptions(multiValuePrepend: String = "")

  // not stack safe but it's ok for our use case
  def toStringFormatted(using options: FormattingOptions = FormattingOptions()): String = this match
    case Text(value)                => value
    case Element(name)              => List(s"<$name>", s"</$name>").mkString("\n")
    case Element(name, Text(value)) => s"""<$name> $value </$name>"""
    case Element(name, values*) =>
      s"""<$name>
           |${values.map(value => options.multiValuePrepend + value.toStringFormatted).mkString("\n")}
           |</$name>""".stripMargin

trait XMLEncoder[A]:
  def encode(value: A): XML
  def contramap[B](fn: B => A): XMLEncoder[B] = value => encode(fn(value))

object XMLEncoder:
  inline def apply[T: XMLEncoder]: XMLEncoder[T] = summon[XMLEncoder[T]]

  extension [A: XMLEncoder](value: A) def encode: XML = summon[XMLEncoder[A]].encode(value)

  def from[A](fn: A => XML): XMLEncoder[A] = fn(_)

  given XMLEncoder[String] = XML.Text(_)
