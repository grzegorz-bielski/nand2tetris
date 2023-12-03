package hasm

class ParserSpec extends munit.FunSuite:
  test("should parse lines with C instructions correctly"):
    Vector(
      "M=D+M" -> Instruction.C(Some("M"), Some("D+M"), None),
      "D=M" -> Instruction.C(Some("D"), Some("M"), None),
      "D=0" -> Instruction.C(Some("D"), Some("0"), None),
      "M=M=1" -> Error.InvalidInstruction("M=M=1"),
      "0;JMP" -> Instruction.C(None, Some("0"), Some("JMP")),
      "D=C;J" -> Instruction.C(Some("D"), Some("C"), Some("J")),
      "A" -> Instruction.C(None, Some("A"), None),
      "-" -> Instruction.C(None, Some("-"), None),
      "d" -> Instruction.C(None, Some("d"), None)
    ).foreach: (line, expected) =>
      assertEquals(Parser.parseLine(line), expected)
