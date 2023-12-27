package jackc

enum XML:
  case Element(name: String, value: XML*)
  case Text(value: String)
  
  val multiValuePrepend = "\t"

  // not stack safe
  def toStringFormatted: String = this match
    case Text(value)            => value
    case Element(name)          => s"<$name>"
    case Element(name, values*) => 
      if values.length == 1 then
        s"""<$name> ${values.map(_.toStringFormatted).mkString} </$name>"""
      else
        s"""<$name>
           |${values.map(value => multiValuePrepend + value.toStringFormatted).mkString("\n")}
           |</$name>""".stripMargin
trait XMLEncoder[A]:
  def encode(value: A): XML
  def contramap[B](fn: B => A): XMLEncoder[B] = value => encode(fn(value))

object XMLEncoder:
  inline def apply[T: XMLEncoder]: XMLEncoder[T] = summon[XMLEncoder[T]]

  extension [A: XMLEncoder](value: A)
    def encode: XML = summon[XMLEncoder[A]].encode(value)

  def from[A](fn: A => XML): XMLEncoder[A] = fn(_)

  given XMLEncoder[String] = XML.Text(_)
