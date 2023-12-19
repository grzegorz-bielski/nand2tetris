package hvm

class HVMSpec extends HVMSuite:
  test("should translate programs to the Hack assembly correctly"):
    assertEquals(
      translateAt(`07` / "MemoryAccess" / "BasicTest" / "BasicTest.vm"),
      expectedAt(`07` / "MemoryAccess" / "BasicTest" / "BasicTest.asm")
    )

    assertEquals(
      translateAt(`07` / "MemoryAccess" / "PointerTest" / "PointerTest.vm"),
      expectedAt(`07` / "MemoryAccess" / "PointerTest" / "PointerTest.asm")
    )

    assertEquals(
      translateAt(`07` / "MemoryAccess" / "StaticTest" / "StaticTest.vm"),
      expectedAt(`07` / "MemoryAccess" / "StaticTest" / "StaticTest.asm")
    )

    assertEquals(
      translateAt(`07` / "StackArithmetic" / "SimpleAdd" / "SimpleAdd.vm"),
      expectedAt(`07` / "StackArithmetic" / "SimpleAdd" / "SimpleAdd.asm")
    )

    assertEquals(
      translateAt(`07` / "StackArithmetic" / "StackTest" / "StackTest.vm"),
      expectedAt(`07` / "StackArithmetic" / "StackTest" / "StackTest.asm")
    )

  def translateAt(path: os.Path) =
    HVM.translate(path.last, Parser.parseAll(path)).map(_.mkString("\n"))

  def expectedAt(path: os.Path) =
    Right(os.read.lines(path).toVector.mkString("\n"))
