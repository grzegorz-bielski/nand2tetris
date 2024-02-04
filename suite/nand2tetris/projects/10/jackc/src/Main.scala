//> using scala 3.3.1
//> using toolkit latest
//> using option -Yexplicit-nulls

package jackc

import scala.util.chaining.*
import scala.util.Using
import scala.io.Source

import XMLEncoder.*
import TokenXML.given
import CompilationEngine.*

@main
def run(source: String) =
  val srcPath = os.Path(source)
  
  if os.isDir(srcPath) then os.list(srcPath).filter(isValidPath).map(compileClassAt)
  else if isValidPath(srcPath) then compileClassAt(srcPath)
  else println(s"Invalid path: $srcPath")

private def isValidPath(path: os.Path) = path.last.startsWith(".") && path.last.endsWith(".jack")

private def compileClassAt(source: os.Path): Unit = 
  Using(Source.fromFile(source.toIO)): 
    _.getLines().pipe(compileClass)
  .toEither.left.map: err =>
    Error.CompilationError(s"Failed to compile $source: $err")
  .joinRight
  .fold(
    println,
    code => os.write.over(source / source.last.replace(".jack", ".vm").nn, code.toString)
  )

private def compileClass(lines: Iterator[String]): Either[Error, VMCode] = 
  Tokenizer.tokenize(lines).flatMap(SyntaxAnalyzer.analyze).flatMap(CompilationEngine.compile)
