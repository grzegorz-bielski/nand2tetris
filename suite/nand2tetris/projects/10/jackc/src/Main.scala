//> using scala 3.3.1
//> using toolkit latest
//> using dep com.lihaoyi::pprint:0.7.0
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

private def isValidPath(path: os.Path) = !path.last.startsWith(".") && path.last.endsWith(".jack")

// assumes `source` is a valid path to a single file
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

private[jackc] def compileClass(lines: Iterator[String]): Either[Error, Vector[VMCode]] = 
  Tokenizer.tokenize(lines).flatMap(SyntaxAnalyzer.analyze).flatMap(CompilationEngine.compile)