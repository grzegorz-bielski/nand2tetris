package jackc

import XMLEncoder.*
import GrammarXML.{*, given}

class SyntaxAnalyzerSpec extends JackcSpec:
  test("should construct AST from a class file correctly"):
    assertEquals(
      analyzeAt(`10` / "ArrayTest" / "Main.jack"),
      expectedAt(`10` / "ArrayTest" / "Main.xml")
    )

    square("ExpressionLessSquare")
    square("Square")

    def square(folder: String) =
      assertEquals(
        analyzeAt(`10` / folder / "Main.jack"),
        expectedAt(`10` / folder / "Main.xml")
      )

      assertEquals(
        analyzeAt(`10` / folder / "Square.jack"),
        expectedAt(`10` / folder / "Square.xml")
      )

      assertEquals(
        analyzeAt(`10` / folder / "SquareGame.jack"),
        expectedAt(`10` / folder / "SquareGame.xml")
      )


  def analyzeDebug(path: os.Path) = 
    lazy val compiled = analyzeAt(path)
    lazy val expected = expectedAt(path / os.up / path.last.replace(".jack", ".xml").nn)

    println(compiled)

    os.write.over(path / os.up / path.last.replace(".jack", "C.xml").nn, compiled.toOption.get)
    os.write.over(path / os.up / path.last.replace(".jack", "C2.xml").nn, expected.toOption.get)

  def analyzeAt(path: os.Path) =
    Tokenizer.tokenizeAt(path)(SyntaxAnalyzer.analyze).joinRight.map(_.encode.toStringFormatted)

  def expectedAt(path: os.Path) =
    Right(os.read.lines(path).map(_.trim.nn).mkString("\n"))
