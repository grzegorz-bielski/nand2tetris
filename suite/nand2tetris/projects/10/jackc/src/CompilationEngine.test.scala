package jackc

import scala.util.chaining.*

import VMCode.*

class CompilationEngineSpec extends JackcSpec:
  test("should compile a class files correctly"):
    assertEquals(
      compileAt(`11` / "Seven" / "Main.jack"),
      expectedAt(`11` / "Seven" / "Main.vm")
    )

    // compileAt(`11` / "ConvertToBin" / "Main.jack").foreach(println)

    assertEquals(
      compileAt(`11` / "ConvertToBin" / "Main.jack"),
      expectedAt(`11` / "ConvertToBin" / "Main.vm")
    )


  def compileAt(path: os.Path) =
    os.read.lines(path).pipe(_.iterator).pipe(compileClass).pipe(_.map(_.collapse))

  def expectedAt(path: os.Path) =
    Right(os.read.lines(path).map(_.trim.nn).mkString("\n"))
