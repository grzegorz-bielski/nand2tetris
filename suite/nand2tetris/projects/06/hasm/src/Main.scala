//> using scala 3.3.1
//> using toolkit latest
//> using option -Yexplicit-nulls

package hasm

// scala-cli ./hasm -- $(pwd)/add/Add $(pwd)/add/kek

@main
def run(source: String, dest: String): Unit =
  HASM.assembleToString(source, dest)
