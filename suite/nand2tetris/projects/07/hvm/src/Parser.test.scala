package hvm

import Command.*

class ParserSpec extends munit.FunSuite:
  test("should parse whole program correctly"):
    assertEquals(
      Parser.parseAll(os.pwd / os.up / os.up / "MemoryAccess" / "BasicTest" / "BasicTest.vm"),
      Vector(
        (0, null),
        (1, null),
        (2, null),
        (3, null),
        (4, null),
        (5, null),
        (6, Push("constant", 10)),
        (7, Pop("local", 0)),
        (8, Push("constant", 21)),
        (9, Push("constant", 22)),
        (10, Pop("argument", 2)),
        (11, Pop("argument", 1)),
        (12, Push("constant", 36)),
        (13, Pop("this", 6)),
        (14, Push("constant", 42)),
        (15, Push("constant", 45)),
        (16, Pop("that", 5)),
        (17, Pop("that", 2)),
        (18, Push("constant", 510)),
        (19, Pop("temp", 6)),
        (20, Push("local", 0)),
        (21, Push("that", 5)),
        (22, Arithmetic("add")),
        (23, Push("argument", 1)),
        (24, Arithmetic("sub")),
        (25, Push("this", 6)),
        (26, Push("this", 6)),
        (27, Arithmetic("add")),
        (28, Arithmetic("sub")),
        (29, Push("temp", 6)),
        (30, Arithmetic("add"))
      )
    )
