package jackc

import scala.util.chaining.*

import VMCode.*

class CompilationEngineSpec extends JackcSpec:
  test("should compile a class files correctly"):
    assertEquals(
      compileAt(`11` / "Seven" / "Main.jack"),
      expectedAt(`11` / "Seven" / "Main.vm")
    )

    // compileAt(`11` / "Average" / "Main.jack").foreach(println)

    assertEquals(
      compileAt(`11` / "ConvertToBin" / "Main.jack"),
      expectedAt(`11` / "ConvertToBin" / "Main.vm") // modified label counters
    )

    assertEquals(
      compileAt(`11` / "Square" / "Main.jack"),
      expectedAt(`11` / "Square" / "Main.vm")
    )

    assertEquals(
      compileAt(`11` / "Square" / "Square.jack"),
      expectedAt(`11` / "Square" / "Square.vm")  // modified label counters
    )

    assertEquals(
      compileAt(`11` / "Square" / "SquareGame.jack"),
      expectedAt(`11` / "Square" / "SquareGame.vm")  // modified label counters
    )

     assertEquals(
      compileAt(`11` / "Average" / "Main.jack"),
      expectedAt(`11` / "Average" / "Main.vm")  // modified label counters
    )


  def compileAt(path: os.Path) =
    os.read.lines(path).pipe(_.iterator).pipe(compileClass).pipe(_.map(_.collapse))

  def expectedAt(path: os.Path) =
    Right(os.read.lines(path).map(_.trim.nn).mkString("\n"))
