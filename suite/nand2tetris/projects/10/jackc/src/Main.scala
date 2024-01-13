//> using scala 3.3.1
//> using toolkit latest
//> using option -Yexplicit-nulls

package jackc

import XMLEncoder.*
import TokenXML.given

// TODO:
// 1. [x] Jack tokenizer
// 2. [ ] Jack compilation engine (syntax analyzer) without expressions and array-oriented statements
// 3. [ ] Jack engine with expressions
// 4. [ ] Jack engine with array-oriented statements

// scala-cli . -- $(pwd)/../ExpressionLessSquare/SquareGame.jack $(pwd)/../ExpressionLessSquare/SquareGameTC.xml
@main
def run(source: String, dest: String) =
  tokenizeToXML(source, dest)

def tokenizeToXML(source: String, dest: String) =
  Tokenizer.tokenize(os.Path(source)) match
    case Left(err)     => println(err)
    case Right(tokens) => os.write.over(os.Path(dest), tokens.encode.toStringFormatted)
