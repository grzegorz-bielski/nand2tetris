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

  def translateToString(source: String, dest: String, includeBootstrap: Boolean = false): Unit =
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
      val asm = result
        .collect:
          case Right(lines) => lines
        .flatten
        .prepended:
          if includeBootstrap then HASMWriter.writeBootstrap else ""
        .mkString("\n")

      val destPath = if isDir then os.Path(dest) / s"$source.asm" else os.Path(s"$dest.asm")

      os.write.over(destPath, asm)

  def translate(path: os.Path): Result =
    Parser.parseAll(path).pipe(translate(path.last, _))

  def translate(fileName: String, sourceLines: Vector[(Int, Parser.Result)]): Result =
    sourceLines.foldLeft(TranslationState.empty(fileName))(_.translateLine(_)).result

  private final case class TranslationState(
      fileName: String,
      parentFunction: Option[ParentFunction],
      results: Vector[HASMWriter.Result]
  ):
    def translateLine(res: (Int, Parser.Result)): TranslationState = res match
      case (_, e: Error) => copy(results = results :+ e)
      case (lineNr, cmd @ Command.Function(name, _)) =>
        writeCmd(cmd, lineNr).copy(parentFunction = Some(ParentFunction(name, 0)))
      case (lineNr, cmd @ Command.Call(_, _)) =>
        writeCmd(cmd, lineNr).copy(parentFunction = parentFunction.map(_.incrementCalls))
      case (lineNr, cmd: Command) => writeCmd(cmd, lineNr)
      case _                      => this

    private def writeCmd(cmd: Command, lineNr: Int): TranslationState =
      copy(results = results :+ HASMWriter.writeCmd(cmd, fileName, lineNr, parentFunction))

    def result: Result =
      val errors = results.collect:
        case e: Error => e

      if errors.nonEmpty then Left(errors) else Right(results.asInstanceOf[Vector[String]])

  private object TranslationState:
    def empty(fileName: String) = TranslationState(
      fileName.dropRight(3), // drop extension
      None,
      Vector.empty
    )

final case class ParentFunction(name: String, callCount: Int):
  def incrementCalls = copy(callCount = callCount + 1)
