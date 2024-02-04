package jackc

import XMLEncoder.*
import TokenXML.given

class TokenizerSpec extends JackcSpec:
  test("should tokenize a class file correctly"):
    
    assertEquals(
      tokenizeAt(`10` / "ArrayTest" / "Main.jack"),
      expectedAt(`10` / "ArrayTest" / "MainT.xml")
    )

    square("ExpressionLessSquare")
    square("Square")

    def square(folder: String) =
      assertEquals(
        tokenizeAt(`10` / folder / "Main.jack"),
        expectedAt(`10` / folder / "MainT.xml")
      )

      assertEquals(
        tokenizeAt(`10` / folder / "Square.jack"),
        expectedAt(`10` / folder / "SquareT.xml")
      )

      assertEquals(
        tokenizeAt(`10` / folder / "SquareGame.jack"),
        expectedAt(`10` / folder / "SquareGameT.xml")
      )

  def tokenizeAt(path: os.Path) =
    Tokenizer.tokenizeAt(path)(_.encode.toStringFormatted)

  def expectedAt(path: os.Path) =
    Right(os.read.lines(path).mkString("\n"))
