package jackc

import XMLEncoder.*
import GrammarXML.{*, given}

class CompilationEngineSpec extends JackcSpec:
  test("should construct AST from a class file correctly"):
    assertEquals(
      compileAt(`10` / "ArrayTest" / "Main.jack"),
      expectedAt(`10` / "ArrayTest" / "Main.xml")
    )

    square("ExpressionLessSquare")
    square("Square")

    def square(folder: String) =
      assertEquals(
        compileAt(`10` / folder / "Main.jack"),
        expectedAt(`10` / folder / "Main.xml")
      )

      assertEquals(
        compileAt(`10` / folder / "Square.jack"),
        expectedAt(`10` / folder / "Square.xml")
      )

      assertEquals(
        compileAt(`10` / folder / "SquareGame.jack"),
        expectedAt(`10` / folder / "SquareGame.xml")
      )


  def compileDebug(path: os.Path) = 
    lazy val compiled = compileAt(path)
    lazy val expected = expectedAt(path / os.up / path.last.replace(".jack", ".xml").nn)

    println(compiled)

    os.write.over(path / os.up / path.last.replace(".jack", "C.xml").nn, compiled.toOption.get)
    os.write.over(path / os.up / path.last.replace(".jack", "C2.xml").nn, expected.toOption.get)

  def compileAt(path: os.Path) =
    Tokenizer.tokenize(path)(CompilationEngine.compile).joinRight.map(_.encode.toStringFormatted)

  def expectedAt(path: os.Path) =
    Right(os.read.lines(path).map(_.trim.nn).mkString("\n"))
