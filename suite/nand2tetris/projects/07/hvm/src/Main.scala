//> using scala 3.3.1
//> using toolkit latest
//> using option -Yexplicit-nulls

package hvm

import scala.util.chaining.*

// scala-cli . -- $(pwd)/../StackArithmetic/StackTest/StackTest  $(pwd)/../StackArithmetic/StackTest/StackTest
@main
def run(source: String, dest: String): Unit =
  HVM.translateToString(source, dest)

object HVM:
  type Result = Either[Vector[Error], Vector[String]]

  def translateToString(source: String, dest: String): Unit =
    val isDir = os.isDir(os.Path(source))
    val result =
      if isDir then
        os.list(os.Path(source))
          .toVector
          .collect:
            case path if path.last.endsWith(".vm") => translate(path)
      else Vector(translate(os.Path(s"$source.vm")))

    val errors = result
      .collect:
        case Left(errors) => errors
      .flatten

    if errors.nonEmpty
    then println(s"Could not translate the program: $errors")
    else
      val asmLines = result
        .collect:
          case Right(lines) => lines
        .flatten
        .mkString("\n")

      val destPath = if isDir then os.Path(dest) / s"$source.asm" else os.Path(s"$dest.asm")

      os.write.over(destPath, asmLines)

  def translate(path: os.Path): Result =
    Parser.parseAll(path).pipe(translate(path.last, _))

  def translate(fileName: String, sourceLines: Vector[(Int, Parser.Result)]): Result =
    val fileNameWithoutExt = fileName.dropRight(3)
    val result = sourceLines.collect:
      case (_, e: Error)          => e
      case (lineNr, cmd: Command) => HASMWriter.writeCmd(cmd, fileNameWithoutExt, lineNr)

    val errors = result.collect:
      case e: Error => e

    if errors.nonEmpty then Left(errors) else Right(result.asInstanceOf[Vector[String]])
