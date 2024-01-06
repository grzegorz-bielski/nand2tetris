package jackc

import XMLEncoder.*

class CompilationEngineSpec extends JackcSpec:
  test("should construct AST from a class file correctly"):

    println(compileAt(`10` / "ArrayTest" / "Main.jack"))

  def compileAt(path: os.Path) =
    Tokenizer.tokenize(path)(CompilationEngine.compile).joinRight
