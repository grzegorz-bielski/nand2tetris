package hasm

class ParserSpec extends munit.FunSuite:
  test("should parse lines with C instructions correctly"):
    Vector(
      "M=D+M" -> Instruction.C(Some("M"), "D+M", None),
      "D=M" -> Instruction.C(Some("D"), "M", None),
      "D=0" -> Instruction.C(Some("D"), "0", None),
      "M=M=1" -> Error.InvalidInstruction("M=M=1"),
      "0;JMP" -> Instruction.C(None, "0", Some("JMP")),
      "D=C;J" -> Instruction.C(Some("D"), "C", Some("J")),
      "A" -> Instruction.C(None, "A", None),
      "-" -> Instruction.C(None, "-", None),
      "d" -> Instruction.C(None, "d", None)
    ).foreach: (line, expected) =>
      assertEquals(Parser.parseLine(line), expected)

  test("should parse whole program correctly"):
    assertEquals(
      Parser.parseAll(os.pwd / os.up / os.up / "add" / "Add.asm"),
      Vector(
        null, // comment
        null, // comment
        null, // comment
        null, // comment
        null, // empty line
        null, // comment
        null, // empty line
        Instruction.A("2"), // @2
        Instruction.C(Some("D"), "A", None), // D=A
        Instruction.A("3"), // @3
        Instruction.C(Some("D"), "D+A", None), // D=D+A
        Instruction.A("0"), // @0
        Instruction.C(Some("M"), "D", None) // M=D
      )
    )
