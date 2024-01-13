package jackc

import XMLEncoder.*
import GrammarXML.{*, given}

class CompilationEngineSpec extends JackcSpec:
  test("should construct AST from a class file correctly"):
    val a = compileAt(`10` / "ExpressionLessSquare" / "Main.jack")

    os.write.over(`10` / "ExpressionLessSquare" / "MainC.xml", a.toOption.get)
    os.write.over(`10` / "ExpressionLessSquare" / "MainC2.xml", expectedAt(`10` / "ExpressionLessSquare" / "Main.xml").toOption.get)

    // assertEquals(
    //   compileAt(`10` / "ArrayTest" / "Main.jack"),
    //   expectedAt(`10` / "ArrayTest" / "Main.xml")
    // )


    // square("ExpressionLessSquare")

    // def square(folder: String) =
    //   assertEquals(
    //     compileAt(`10` / folder / "Main.jack"),
    //     expectedAt(`10` / folder / "Main.xml")
    //   )

    //   // assertEquals(
    //   //   compileAt(`10` / folder / "Square.jack"),
    //   //   expectedAt(`10` / folder / "SquareT.xml")
    //   // )

    //   // assertEquals(
    //   //   compileAt(`10` / folder / "SquareGame.jack"),
    //   //   expectedAt(`10` / folder / "SquareGameT.xml")
    //   // )


  def compileAt(path: os.Path) =
    Tokenizer.tokenize(path)(CompilationEngine.compile).joinRight.map(_.encode.toStringFormatted)

  def expectedAt(path: os.Path) =
    Right(os.read.lines(path).map(_.trim.nn).mkString("\n"))
