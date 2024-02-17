package jackc

import scala.util.chaining.*

import VMCode.*

class CompilationEngineSpec extends JackcSpec:
  test("should compile `Seven` correctly"):
    assertEquals(
      compileAt(`11` / "Seven" / "Main.jack"),
      expectedAt(`11` / "Seven" / "Main.vm")
    )

  test("should compile `ConvertToBin` correctly"):
    assertEquals(
      compileAt(`11` / "ConvertToBin" / "Main.jack"),
      expectedAt(`11` / "ConvertToBin" / "Main.vm") // modified label counters
    )

  test("should compile `Square` correctly"):
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

  test("should compile `Average` correctly"):
    assertEquals(
      compileAt(`11` / "Average" / "Main.jack"),
      expectedAt(`11` / "Average" / "Main.vm")  // modified label counters
    )

  test("should compile `Pong` correctly"):
    assertEquals(
      compileAt(`11` / "Pong" / "Main.jack"),
      expectedAt(`11` / "Pong" / "Main.vm")
    )

    assertEquals(
      compileAt(`11` / "Pong" / "Ball.jack"),
      expectedAt(`11` / "Pong" / "Ball.vm") // modified label counters
    )

    assertEquals(
      compileAt(`11` / "Pong" / "Bat.jack"),
      expectedAt(`11` / "Pong" / "Bat.vm")
    )

    assertEquals(
      compileAt(`11` / "Pong" / "PongGame.jack"),
      expectedAt(`11` / "Pong" / "PongGame.vm") // modified label counters
    )

  // test("should compile `ComplexArrays` correctly"):
  //   compileAt(`11` / "Pong" / "ComplexArrays.jack").foreach(println)
    
  //   assertEquals(
  //     compileAt(`11` / "ComplexArrays" / "Main.jack"),
  //     expectedAt(`11` / "ComplexArrays" / "Main.vm")
  //   )


  def compileAt(path: os.Path) =
    os.read.lines(path).pipe(_.iterator).pipe(compileClass).pipe(_.map(_.collapse))

  def expectedAt(path: os.Path) =
    Right(os.read.lines(path).map(_.trim.nn).mkString("\n"))
